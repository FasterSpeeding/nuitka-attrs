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

__all__: typing.List[str] = [
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
import importlib
import inspect
import optparse
import re
import sys
import textwrap
import types
import typing

import attrs
from nuitka.plugins import PluginBase as plugin_base
from nuitka.utils import ModuleNames as module_names

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

ATTRS_CLS_GEN: typing.FrozenSet[str] = frozenset(
    (
        # old "attr" namespace
        "attr.attributes",
        "attr.attrs",
        "attr.define",
        "attr.frozen",
        "attr.mutable",
        "attr.s",
        # new "attrs" namespace
        "attrs.define",
        "attrs.frozen",
        "attrs.mutable",
    )
)
_ATTRS_FIELD_GEN: typing.FrozenSet[str] = frozenset(
    (
        # old "attr" namespace
        "attr.attr",
        "attr.attrib",
        "attr.ib"
        # new "attrs" namespace
        "attrs.field",
    )
)
# __str__, __getstate__ and __setstate__ can be ignored as these aren't procedurally generated.
_ATTRS_GENNED_METHODS: typing.Sequence[str] = [
    "__init__",
    "__repr__ ",
    "__eq__",
    "__ne__",
    "__lt__",
    "__le__",
    "__gt__",
    "__ge__",
    "__hash__",
]
_ATTR_PATHS: typing.FrozenSet[str] = frozenset((*ATTRS_CLS_GEN, *_ATTRS_FIELD_GEN, "attr", "attrs"))
_FACTORY_PATHS = ["attr.Factory", "attrs.Factory"]
_NOTHING_PATHS = ["attr.NOTHING", "attrs.NOTHING"]


class _Module:
    __slots__ = ("classes", "imports", "lines_to_insert", "module", "node", "plugin", "source_code")

    def __init__(self, plugin: AttrsPlugin, module: types.ModuleType, source_code: str, /):
        self.classes: typing.Dict[str, _Class] = {}
        self.imports: typing.Dict[str, str] = {}
        self.lines_to_insert: typing.List[typing.Tuple[int, str]] = []
        self.module = module
        self.plugin = plugin
        self.source_code = source_code.split("\n")

        node = ast.parse(source_code)
        assert isinstance(node, ast.Module)
        self.node = node
        self._process_nodes(node.body)

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

    def _process_nodes(self, nodes: typing.Sequence[ast.AST], /) -> None:
        for node in nodes:
            self._process_node(node)

    def add_import(self, path: str, /, *, hint: int = -1) -> str:
        raise NotImplementedError

    def get_import(self, path: str, /) -> typing.Optional[str]:
        split = path.split(".")
        for i in range(len(split)):
            maybe_path = (".".join(split[i:]) + "." + ".".join(split[:i])).strip(". ")
            if self.match_to_import(maybe_path):
                return maybe_path

    def match_to_import(self, path: str, /) -> typing.Optional[str]:
        split = path.split(".")
        for imported_as, full_path in self.imports.items():
            split_at = imported_as.count(".") + 1
            if ".".join(split[:split_at]) == imported_as:
                return (full_path + "." + ".".join(split[split_at:])).rstrip(".")

    def join(self) -> str:
        if not self.lines_to_insert:
            return "\n".join(self.source_code)

        self.lines_to_insert = sorted(self.lines_to_insert, key=lambda v: v[0], reverse=True)
        source_code = self.source_code.copy()
        for line_number, line in self.lines_to_insert:
            source_code.insert(line_number, line)

        return "\n".join(source_code)

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


def _get_ast_default(
    module: _Module, node: ast.AST, /
) -> typing.Tuple[typing.Optional[ast.AST], typing.Optional[ast.AST]]:
    if isinstance(node, ast.Call):
        import_path = module.match_to_import(_try_parse_path(node.func))
        if import_path not in _ATTRS_FIELD_GEN:
            for arg in node.keywords:
                if arg.arg == "default":
                    return (arg.value, None)

                if arg.arg == "factory":
                    return (None, arg.value)

    return (node, None)


_EMPTY = object()


def _uninherited_method(cls: type[typing.Any], attribute: str, /) -> typing.Optional[types.FunctionType]:
    value = getattr(cls, attribute, _EMPTY)
    if value is _EMPTY:
        return None

    for base_cls in cls.mro()[1:]:
        other_value = getattr(base_cls, attribute, None)
        if value is other_value:
            return None

    return value if isinstance(value, types.FunctionType) else None


class _Attr:
    def __init__(self, default: typing.Optional[ast.AST] = None, factory: typing.Optional[ast.AST] = None) -> None:
        self.default = default  # TODO: attr_dict
        self.factory = factory  # TODO: NOTHING


def _filter_map(
    callback: typing.Callable[[_T], typing.Optional[_OtherT]], iterable: typing.Iterable[typing.Optional[_T]]
) -> typing.Iterator[_OtherT]:
    return filter(None, map(callback, filter(None, iterable)))


class _Class:
    __slots__ = ("_cls", "declared_methods", "fields", "module", "node")

    def __init__(self, module: _Module, cls_node: ast.ClassDef, /) -> None:
        self._cls: typing.Union[_EllipsisType, typing.Type[typing.Any], None] = ...
        self.declared_methods: typing.Set[str] = set()
        self.fields: typing.Dict[str, _Attr] = {}
        self.module = module
        self.node = cls_node

        for node in cls_node.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                self.declared_methods.add(node.name)

            elif isinstance(node, ast.Assign):
                # TODO: do we always want to assume auto?
                defaults = _get_ast_default(self.module, node.value) or (node.value, None)
                for target in node.targets:
                    if isinstance(target, ast.Name):
                        self.fields[target.id] = _Attr(*defaults)

            elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
                defaults = _get_ast_default(self.module, node.value) if node.value else (None, None)
                self.fields[node.target.id] = _Attr(*defaults)

    def get_cls(self) -> typing.Optional[typing.Type[typing.Any]]:
        if self._cls is ...:
            cls = getattr(self.module.module, self.node.name, None)
            assert cls is None or isinstance(cls, type)
            self._cls = cls

        assert not isinstance(self._cls, _EllipsisType)
        return self._cls

    def process(self) -> None:
        module = self.module
        match = next(_filter_map(module.match_to_import, _filter_map(_try_parse_path, self.node.decorator_list)), None)
        cls = self.get_cls()
        if match not in ATTRS_CLS_GEN or not cls:
            return

        for base_cls in cls.mro()[:-1]:
            try:
                attrs.fields(base_cls)
            except attrs.exceptions.NotAnAttrsClassError:
                continue

            other_module = self.module.plugin.get_module(base_cls.__module__)

            # TODO: deal with making sure any names the other fields use are available in the global
            # namespace
            if other_class := other_module.classes.get(base_cls.__name__):
                self.fields = other_class.fields | self.fields

        nothing_import = next(_filter_map(module.get_import, _NOTHING_PATHS), None) or module.add_import(
            _NOTHING_PATHS[0]
        )
        before_class = (self.node.decorator_list[0].lineno if self.node.decorator_list else self.node.lineno) - 1
        # Skip the class docstring
        start_of_cls = self.node.body[0]
        line = module.source_code[start_of_cls.lineno - 1]
        indent = line[: len(line) - len(line.lstrip())]
        for attribute in _ATTRS_GENNED_METHODS:
            method = _uninherited_method(cls, attribute)
            if method is None or attribute in self.declared_methods:
                continue

            code = textwrap.indent(textwrap.dedent(inspect.getsource(method)).replace("    ", indent), indent)

            if attribute == "__init__":
                code = code.replace("_cached_setattr.__get__", "object.__setattr__.__get__").replace(
                    "NOTHING", nothing_import
                )
                for name, field in self.fields.items():
                    if field.default:
                        code = code.replace(f"attr_dict[{name!r}].default", ast.unparse(field.default))

                    elif field.factory:
                        factory_name = f"__attr_{cls.__name__}__init__{name}_factory"
                        module.lines_to_insert.append((before_class, factory_name + " = " + ast.unparse(field.factory)))
                        code = code.replace(f"__attr_factory_{name}", factory_name)

            module.lines_to_insert.append((start_of_cls.end_lineno or start_of_cls.lineno, code))


class AttrsPlugin(plugin_base.NuitkaPluginBase):
    """A Nuitka plugin for compile-time generating attrs and dataclasses methods."""

    plugin_name: typing.Final[str] = "attrs"

    def __init__(self, attrs_modules: typing.Optional[typing.List[str]]) -> None:
        self.attrs_modules = [re.compile(name) for name in attrs_modules] if attrs_modules else None
        self.module_cache: typing.Dict[str, _Module] = {}

    @classmethod
    def addPluginCommandLineOptions(cls, group: optparse.OptionGroup) -> None:
        group.add_option(
            "--attrs-module",
            action="append",
            dest="attrs_modules",
            default=None,
            help="Modules to pre-compile the attrs class methods for. These are regex matched.",
        )

    def _match_module(self, module: str, /) -> bool:
        if self.attrs_modules is None:
            return True

        return module in self.attrs_modules

    def get_module(self, name: str, /, source_code: typing.Optional[str] = None) -> _Module:
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

    def onModuleSourceCode(self, module_name: module_names.ModuleName, source_code: str) -> str:
        if not self._match_module(module_name):
            return source_code

        try:
            module = self.get_module(str(module_name))
        except Exception as exc:
            self.info(f"Failed to load {module_name!s} module to process dataclasses: {exc}")
            return source_code

        module.process()
        return module.join()
