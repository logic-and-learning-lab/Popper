from typing import Dict, Any


try:
    from pkg_resources import resource_string, resource_filename
    def close_resource_file(ref: Any):
        pass
except:
    import importlib.resources
    from contextlib import ExitStack
    from importlib.resources.abc import Traversable
    def resource_string(package: str, path: str) -> bytes:
        ref = importlib.resources.files(package).joinpath(path)
        with ref.open('rb') as fp:
            my_bytes = fp.read()
        return my_bytes
    managers: Dict[Traversable, ExitStack] = {}
    def resource_filename(pkg: str, path: str) -> Traversable:
        manager = ExitStack()
        ref = manager.enter_context(importlib.resources.files(pkg) / path)
        managers[ref] = manager
        return ref
    def close_resource_file(ref: Traversable):
        if ref in managers:
            managers[ref].close()
            del managers[ref]
