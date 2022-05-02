# -*- coding: utf-8 -*-
# cython: language_level=3
# BSD 3-Clause License
#
# Copyright (c) 2022, Faster Speeding
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""A Nutika plugin for compile-time generating attrs and dataclasses methods."""
from __future__ import annotations

__all__: list[str] = [
    "AttrsPlugin",
    "__author__",
    "__ci__",
    "__copyright__",
    "__coverage__",
    "__docs__",
    "__email__",
    "__issue_tracker__",
    "__license__",
    "__url__",
    "__version__",
]

import ast
import dataclasses
import importlib
import inspect
import optparse
import pathlib
import re
import sys
import textwrap
import types
import typing
from collections import abc as collections

import attrs
from nuitka.plugins import PluginBase as plugin_base  # type: ignore
from nuitka.utils import ModuleNames as module_names  # type: ignore

__author__: typing.Final[str] = "Faster Speeding"
__ci__: typing.Final[str] = "https://github.com/FasterSpeeding/nuitka_attrs/actions"
__copyright__: typing.Final[str] = "© 2022 Faster Speeding"
__coverage__: typing.Final[str] = "https://codeclimate.com/github/FasterSpeeding/nuitka_attrs"
__docs__: typing.Final[str] = "https://tanjun.cursed.solutions/"
__email__: typing.Final[str] = "lucina@lmbyrne.dev"
__issue_tracker__: typing.Final[str] = "https://github.com/FasterSpeeding/nuitka_attrs/issues"
__license__: typing.Final[str] = "BSD"
__url__: typing.Final[str] = "https://github.com/FasterSpeeding/nuitka_attrs"
__version__: typing.Final[str] = "0.1.0"

_T = typing.TypeVar("_T")
_OtherT = typing.TypeVar("_OtherT")

if sys.version_info >= (3, 10):
    _EllipsisType = types.EllipsisType

else:
    _EllipsisType = type(...)

ATTRS_CLS_GEN = frozenset(
    (
        # old "attr" namespace
        "attr.attributes",
        "attr.attrs",
        "attr.define",
        "attr.frozen",
        "attr.mutable",
        "attr.s",
        "_make.attrib",  # This is used internally by attrs.
        "_make.attrs",  # This is used internally by attrs.
        # new "attrs" namespace
        "attrs.define",
        "attrs.frozen",
        "attrs.mutable",
    )
)
_ATTRS_FIELD_GEN = frozenset(
    (
        # old "attr" namespace
        "attr.attr",
        "attr.attrib",
        "attr.field",
        "attr.ib",
        # new "attrs" namespace
        "attrs.field",
    )
)
# "__le__", __lt__, "__gt__", "__ge__", "__ne__", "__str__", "__getstate__" and "__setstate__"
# can be ignored as these aren't procedurally generated.
# As a note, repr is generated on <= 3.6 to take advantage of f-string support.
_ATTRS_GET_DISABLED_METHODS = ["attr.validators.get_disabled", "attrs.validators.get_disabled"]
_ATTR_PATHS = frozenset((*ATTRS_CLS_GEN, *_ATTRS_FIELD_GEN, "attr", "attrs"))
_NOTHING_PATHS = ["attr.NOTHING", "attrs.NOTHING"]


class _Module:
    __slots__ = ("classes", "imports", "lines_to_insert", "module", "node", "plugin", "source_code")

    def __init__(self, plugin: AttrsPlugin, module: types.ModuleType, source_code: str, /):
        self.classes: dict[str, _Class] = {}
        self.imports: dict[str, str] = {}
        self.lines_to_insert: list[tuple[int, str]] = []
        self.module = module
        self.node = ast.parse(source_code)
        self.plugin = plugin
        self.source_code = source_code.split("\n")
        self._process_nodes(self.node.body)

    def _process_if(self, node: ast.If, /):
        self._process_nodes(node.body)
        if isinstance(node.orelse, ast.If):
            self._process_if(node.orelse)

        else:
            self._process_nodes(node.orelse)

    def _process_node(self, node: ast.AST, /) -> None:
        if isinstance(node, ast.ClassDef):
            self.classes[node.name] = _Class(self, node)

        # elif isinstance(node, ast.If):
        #     self._process_if(module, node)

        elif isinstance(node, ast.With):
            self._process_nodes(node.body)

        # elif isinstance(node, ast.Try):
        #     self._process_nodes(module, node.body)

        #     for handler in node.handlers:
        #         self._process_nodes(module, handler.body)

        #     self._process_nodes(module, node.orelse)
        #     self._process_nodes(module, node.finalbody)

        elif isinstance(node, ast.Import):
            for name in node.names:
                if name.name in _ATTR_PATHS:
                    self.imports[name.asname or name.name] = name.asname or name.name

        elif isinstance(node, ast.ImportFrom):
            for name in node.names:
                full_name = f"{node.module}.{name.name}"
                if full_name in _ATTR_PATHS:
                    self.imports[name.asname or name.name] = name.asname or name.name

    def _process_nodes(self, nodes: collections.Sequence[ast.AST], /) -> None:
        for node in nodes:
            self._process_node(node)

    def add_import(self, path: str, /, *, hint: int = -1) -> str:
        raise NotImplementedError

    def get_import(self, path: str, /) -> str | None:
        split = path.split(".")
        for i in range(len(split)):
            maybe_path = (".".join(split[i:]) + "." + ".".join(split[:i])).strip(". ")
            if self.match_to_import(maybe_path):
                return maybe_path

    def match_to_import(self, path: str, /) -> str | None:
        split = path.split(".")
        for imported_as, full_path in self.imports.items():
            split_at = imported_as.count(".") + 1
            if ".".join(split[:split_at]) == imported_as:
                return (full_path + "." + ".".join(split[split_at:])).rstrip(".")

    def join(self) -> str | None:
        if not self.lines_to_insert:
            return

        self.lines_to_insert = sorted(self.lines_to_insert, key=lambda v: v[0], reverse=True)
        source_code = self.source_code.copy()
        for line_number, line in self.lines_to_insert:
            source_code.insert(line_number, line)

        result = "\n".join(source_code)
        return result

    def process(self) -> None:
        for cls in self.classes.values():
            cls.process()


def _try_parse_path(node: ast.AST, /) -> str:
    if isinstance(node, ast.Call):
        return _try_parse_path(node.func)

    if isinstance(node, ast.Attribute):
        return f"{_try_parse_path(node.value)}.{node.attr}".lstrip(".")

    if isinstance(node, ast.Name):
        return node.id

    return ""


_EMPTY = object()


def _uninherited_method(cls: type[typing.Any], attribute: str, /) -> types.FunctionType | None:
    value = getattr(cls, attribute, _EMPTY)
    if value is _EMPTY:
        return None

    for base_cls in cls.mro()[1:]:
        other_value = getattr(base_cls, attribute, _EMPTY)
        if value is other_value:
            return None

    return value if isinstance(value, types.FunctionType) else None


_FieldKwargT = typing.Literal[
    "cmp", "converter", "default", "eq", "factory", "hash", "on_setattr", "order", "repr", "validator"
]
_FIELD_KWARGS: frozenset[_FieldKwargT] = frozenset(
    ("cmp", "converter", "default", "eq", "factory", "hash", "on_setattr", "order", "repr", "validator")
)
_SPECIAL_CASED_DECORATORS = {"default": "factory"}


@dataclasses.dataclass(slots=True)
class _Field:
    cmp: ast.AST | str | None = None
    converter: ast.AST | str | None = None
    default: ast.AST | None = None  # TODO: attr_dict
    eq: ast.AST | str | None = None
    factory: ast.AST | str | None = None  # TODO: NOTHING
    hash: ast.AST | None = None
    on_setattr: ast.AST | str | None = None
    order: ast.AST | str | None = None
    repr: ast.AST | str | None = None
    validator: ast.AST | str | None = None

    @classmethod
    def from_assign(cls, module: _Module, node: ast.Assign | ast.AnnAssign) -> "_Field":
        if not node.value or not isinstance(node.value, ast.Call):
            return _Field(default=node.value)

        import_path = module.match_to_import(_try_parse_path(node.value.func))
        if import_path not in _ATTRS_FIELD_GEN:
            return cls(default=node.value)

        kwargs = {arg.arg: arg.value for arg in node.value.keywords if arg.arg in _FIELD_KWARGS}
        return cls(**kwargs)


def _filter_map(
    callback: collections.Callable[[_T], _OtherT | None], iterable: collections.Iterable[_T | None]
) -> collections.Iterator[_OtherT]:
    return filter(None, map(callback, filter(None, iterable)))


class _Class:
    __slots__ = ("_cls", "declared_methods", "fields", "kwargs_lines", "module", "node")

    def __init__(self, module: _Module, cls_node: ast.ClassDef, /) -> None:
        self._cls: _EllipsisType | type[typing.Any] | None = ...
        self.declared_methods: set[str] = set()
        self.fields: dict[str, _Field] = {}
        self.kwargs_lines: typing.Optional[range] = None
        self.module = module
        self.node = cls_node

        field_modifiers: dict[str, set[tuple[str, str]]] = {}

        for node in cls_node.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                self.declared_methods.add(node.name)
                for decorator in node.decorator_list:
                    if isinstance(decorator, ast.Attribute) and isinstance(decorator.value, ast.Name):
                        if decorator.value.id not in field_modifiers:
                            field_modifiers[decorator.value.id] = set()

                        field_modifiers[decorator.value.id].add((decorator.attr, node.name))

            elif isinstance(node, ast.Assign):
                # TODO: do we always want to assume auto?
                field = _Field.from_assign(self.module, node)
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        self.fields[target.id] = field

            elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
                self.fields[node.target.id] = _Field.from_assign(self.module, node)

        for name, modifiers in field_modifiers.items():
            if not (field := self.fields.get(name)):
                continue

            for modifier, attribute in modifiers:
                modifier = _SPECIAL_CASED_DECORATORS.get(modifier, modifier)
                if modifier in _FIELD_KWARGS:
                    setattr(field, modifier, f"self.{attribute}")

    @typing.overload
    def get_cls(self, *, assert_exists: typing.Literal[True]) -> type[typing.Any]:
        ...

    @typing.overload
    def get_cls(self, *, assert_exists: typing.Literal[False] = False) -> type[typing.Any] | None:
        ...

    def get_cls(self, *, assert_exists: bool = False) -> type[typing.Any] | None:
        if self._cls is ...:
            cls = getattr(self.module.module, self.node.name, None)
            assert cls is None or isinstance(cls, type)
            self._cls = cls

        assert not isinstance(self._cls, _EllipsisType)
        if assert_exists:
            assert self._cls is not None

        return self._cls

    def process(self) -> None:
        module = self.module
        matches = _filter_map(module.match_to_import, _filter_map(_try_parse_path, self.node.decorator_list))
        cls = self.get_cls()
        if not cls or not any(match in ATTRS_CLS_GEN for match in matches):
            return

        for base_cls in cls.mro()[:-1]:
            try:
                attrs.fields(base_cls)
            except attrs.exceptions.NotAnAttrsClassError:
                continue

            other_module = self.module.plugin.get_module_cached(base_cls.__module__)

            # TODO: deal with making sure any names the other fields use are available in the global
            # namespace
            if other_class := other_module.classes.get(base_cls.__name__):
                self.fields = other_class.fields | self.fields

        nothing_import = next(_filter_map(module.get_import, _NOTHING_PATHS), None) or module.add_import(
            _NOTHING_PATHS[0]
        )

        if self.node.lineno == self.node.end_lineno:
            # TODO: handle 1 line classes.
            raise NotImplementedError

        else:
            before_class = (self.node.decorator_list[0].lineno if self.node.decorator_list else self.node.lineno) - 1
            # Skip the class docstring
            start_of_body = self.node.body[0]
            indent = module.source_code[start_of_body.lineno - 1].removesuffix(
                module.source_code[start_of_body.lineno - 1].lstrip()
            )

        for attribute, callback in _ATTRS_GENNED_METHODS.items():
            method = _uninherited_method(cls, attribute)
            if method is None or attribute in self.declared_methods:
                continue

            code = textwrap.dedent(inspect.getsource(method)).replace("    ", indent)
            code = callback(code, self, nothing_import, before_class, start_of_body, indent)
            module.lines_to_insert.append(
                (start_of_body.end_lineno or start_of_body.lineno, textwrap.indent(code, indent))
            )

    def replace_decorator_kwarg(self, name: str):
        if self.kwargs_lines is None:
            decorators = map(self.module.match_to_import, map(_try_parse_path, self.node.decorator_list))
            for decorator_node, decorator in zip(self.node.decorator_list, decorators):
                if decorator in ATTRS_CLS_GEN:
                    self.kwargs_lines = range(
                        decorator_node.lineno - 1, decorator_node.end_lineno or decorator_node.lineno
                    )
                    break

        assert self.kwargs_lines is not None
        for line in self.kwargs_lines:
            self.module.source_code[line] = self.module.source_code[line].replace("{name}=True", "{name}=False")


def _process_any_init(
    code: str, cls_data: _Class, nothing_import: str, before_class: int, _: ast.stmt, __: str, /
) -> str:
    cls = cls_data.get_cls(assert_exists=True)
    validator_import = next(
        _filter_map(cls_data.module.get_import, _ATTRS_GET_DISABLED_METHODS), None
    ) or cls_data.module.add_import(_ATTRS_GET_DISABLED_METHODS[0])
    code = (
        code.replace("_cached_setattr.__get__", "object.__setattr__.__get__")
        .replace("NOTHING", nothing_import)
        .replace("_config._run_validators is True", f"{validator_import}() is False")
    )
    for field_index, (name, field) in enumerate(attrs.fields_dict(cls).items()):
        field_variable = None

        if isinstance(field.default, attrs.Factory):  # type: ignore  # attrs typing bug
            code, field_variable = _add_get_field(field_variable, code, field_index, name)
            code = code.replace(f"__attr_factory_{name}", f"{field_variable}.default.factory")

        elif field.default is not attrs.NOTHING:
            # TODO: we need to calculate pre-indent
            # TODO: we need to handle imports the default may be using
            default_name = f"_attr_{cls.__name__}_{name}_default"
            default = cls_data.fields[name].default
            assert default is not None
            cls_data.module.lines_to_insert.append((before_class, f"{default_name} = {ast.unparse(default)}"))
            code = code.replace(f"attr_dict[{name!r}].default", default_name)

        if field.converter:
            code, field_variable = _add_get_field(field_variable, code, field_index, name)
            code = code.replace(f"__attr_converter_{name}", f"{field_variable}.converter")

        if field.validator:
            code, field_variable = _add_get_field(field_variable, code, field_index, name)
            code = code.replace(f"__attr_validator_{name}", f"{field_variable}.validator").replace(
                f"__attr_{name}", field_variable
            )

    return code


def _process_init(
    code: str, cls_data: _Class, nothing_import: str, before_class: int, start_of_body: ast.stmt, indent: str, /
) -> str:
    cls_data.replace_decorator_kwarg("init")
    # Hack to stop `__attrs_init__` from being unwantedly generated
    pre_indent = cls_data.module.source_code[cls_data.node.lineno - 1].removesuffix(
        cls_data.module.source_code[cls_data.node.lineno - 1].lstrip()
    )
    cls = cls_data.get_cls(assert_exists=True)
    if cls_data.node.end_lineno is None:
        # This doesn't seem to ever actually happen so i can't safely make any assumptions here.
        raise NotImplementedError(f"{cls_data.module.module.__name__}.{cls.__name__}")

    if not _uninherited_method(cls, "__attrs_init__"):
        cls_data.module.lines_to_insert.extend(
            (
                (
                    start_of_body.end_lineno or start_of_body.lineno,
                    f"{indent}__attrs_init__ = lambda *args, **kwargs: None",
                ),
                (cls_data.node.end_lineno + 1, f"{pre_indent}del {cls_data.node.name}.__attrs_init__"),
            )
        )
    return _process_any_init(code, cls_data, nothing_import, before_class, start_of_body, indent)


def _process_eq(
    code: str, cls_data: _Class, nothing_import: str, before_class: int, start_of_body: ast.stmt, indent: str, /
) -> str:
    cls_data.replace_decorator_kwarg("eq")
    return code


def _process_hash(
    code: str, cls_data: _Class, nothing_import: str, before_class: int, start_of_body: ast.stmt, indent: str, /
) -> str:
    cls_data.replace_decorator_kwarg("hash")
    return code


def _process_repr(
    code: str, cls_data: _Class, nothing_import: str, before_class: int, start_of_body: ast.stmt, indent: str, /
) -> str:
    cls_data.replace_decorator_kwarg("repr")
    return code


_ATTRS_GENNED_METHODS: dict[str, collections.Callable[[str, _Class, str, int, ast.stmt, str], str]] = {
    "__attrs_init__": _process_any_init,
    "__eq__": _process_eq,
    "__hash__": _process_hash,
    "__init__": _process_init,
    "__repr__": _process_repr,
}


def _add_get_field(variable_name: str | None, code: str, index: int, name: str) -> tuple[str, str]:
    if variable_name:
        return code, variable_name

    variable_name = f"__attrs_field_{name}"
    split_code = code.split("\n")
    nodes = ast.parse(code).body[0]
    assert isinstance(nodes, ast.FunctionDef)
    first_line = nodes.body[0].lineno - 1
    indent = split_code[first_line].removesuffix(split_code[first_line].lstrip())
    split_code.insert(first_line, f"{indent}{variable_name} = self.__attrs_attrs__[{index}]")
    return "\n".join(split_code), variable_name


class AttrsPlugin(plugin_base.NuitkaPluginBase):
    """A Nuitka plugin for compile-time generating attrs and dataclasses methods."""

    plugin_name: typing.Final[str] = "nuitka_attrs"

    def __init__(self, attrs_modules: list[str] | None, debug_dir: str | None) -> None:
        self.attrs_modules = [re.compile(name) for name in attrs_modules] if attrs_modules else None
        self.debug_dir = pathlib.Path(debug_dir) if debug_dir else None
        self.module_cache: dict[str, _Module] = {}

        if self.debug_dir:
            for file in self.debug_dir.glob("*-debug.py"):
                file.unlink()

    @classmethod
    def addPluginCommandLineOptions(cls, group: optparse.OptionGroup) -> None:
        group.add_option(
            "--attrs-module",
            action="append",
            dest="attrs_modules",
            default=None,
            help="Modules to pre-compile the attrs class methods for. These are regex matched.",
        )
        group.add_option(
            "--attrs-debug-dir",
            action="store",
            dest="debug_dir",
            default=None,
            help="A debug directory that the post attrs-handling generated modules are saved to."
            "Warning, all .py files in this directory will be overwritten or deleted.",
        )

    def get_module(self, name: str, /, *, source_code: str | None = None) -> _Module:
        if module := self.module_cache.get(name):
            self.module_cache[name] = self.module_cache.pop(name)
            return module

        module = importlib.import_module(name)

        if source_code is None:
            if not module.__file__:
                raise RuntimeError(f"Module {name} has no __file__")

            with open(module.__file__, "r") as file:
                source_code = file.read()

        module = self.module_cache[name] = _Module(self, module, source_code)
        if len(self.module_cache) > 1000:
            self.module_cache.pop(next(iter(self.module_cache)))

        return module

    def get_module_cached(self, name: str, /, *, source_code: str | None = None) -> _Module:
        if module := self.module_cache.get(name):
            self.module_cache[name] = self.module_cache.pop(name)
            return module

        return self.get_module(name, source_code=source_code)

    def onModuleSourceCode(self, module_name: module_names.ModuleName, source_code: str) -> str:
        name = str(module_name)

        force_write = False
        if module_name == "attr._make":
            # Small hack to stop attrs from generating `__attrs_init__` because of the generated `__init__`.
            source_code = source_code.replace(
                """if _determine_whether_to_implement(
            cls, init, auto_detect, ("__init__",)
        ):
            builder.add_init()
        else:""",
                """if _determine_whether_to_implement(
            cls, init, auto_detect, ("__init__",)
        ):
            builder.add_init()
        elif not _has_own_attribute(cls, "__attrs_init__"):""",
            ).replace("a = getattr(base_cls, attrib_name, None)", "a = getattr(base_cls, attrib_name, _sentinel)")
            force_write = True

        elif self.attrs_modules and not any(match.fullmatch(name) for match in self.attrs_modules):
            return source_code

        try:
            module = self.get_module(name, source_code=source_code)
        except Exception as exc:
            self.info(f"Failed to load {name!s} module to process dataclasses: {exc}")
            return source_code

        module.process()

        result = module.join()
        self._write_debug(module_name, (result or source_code) if force_write else result)
        return result or source_code

    def _write_debug(self, module_name: module_names.ModuleName, source_code: str | None) -> None:
        if self.debug_dir and source_code:
            with (self.debug_dir / f"{module_name!s}-debug.py").open("w+") as file:
                file.write(source_code)
