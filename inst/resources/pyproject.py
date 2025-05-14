"""
Support for detecting various information from python projects metadata.

Metadata can only be loaded from static files (e.g. pyproject.toml, setup.cfg, etc.)
but not from setup.py due to its dynamic nature.
"""

import configparser
import pathlib
import re

try:
    import typing
except ImportError:
    typing = None

try:
    import tomllib
except ImportError:
    # Python 3.11+ has tomllib in the standard library
    import toml as tomllib  # type: ignore[no-redef]

PEP440_OPERATORS_REGEX = r"(===|==|!=|<=|>=|<|>|~=)"
VALID_VERSION_REQ_REGEX = rf"^({PEP440_OPERATORS_REGEX}?\d+(\.[\d\*]+)*)+$"


def detect_python_version_requirement(directory):
    # type: (typing.Union[str, pathlib.Path]) -> typing.Optional[str]
    """Detect the python version requirement for a project.

    The directory should contain a metadata file such as pyproject.toml,
    setup.cfg, or .python-version.

    Returns the python version requirement as a string or None if not found.
    """
    for _, metadata_file in lookup_metadata_file(directory):
        parser = get_python_version_requirement_parser(metadata_file)
        try:
            version_constraint = parser(metadata_file)
        except InvalidVersionConstraintError as err:
            # Invalid python version constraint in file, ignoring it
            continue

        if version_constraint:
            return version_constraint

    return None


def lookup_metadata_file(directory):
    # type: (typing.Union[str, pathlib.Path]) -> typing.List[typing.Tuple[str, pathlib.Path]]
    """Given the directory of a project return the path of a usable metadata file.

    The returned value is either a list of tuples [(filename, path)] or
    an empty list [] if no metadata file was found.

    The metadata files are returned in the priority they should be processed
    to determine the python version requirements.
    """
    directory = pathlib.Path(directory)

    def _generate():
        for filename in (".python-version", "pyproject.toml", "setup.cfg"):
            path = directory / filename
            if path.is_file():
                yield (filename, path)

    return list(_generate())


def get_python_version_requirement_parser(metadata_file):
    # type: (pathlib.Path) -> typing.Callable[[pathlib.Path], typing.Optional[str]]
    """Given the metadata file, return the appropriate parser function.

    The returned function takes a pathlib.Path and returns the parsed value.
    """
    if metadata_file.name == "pyproject.toml":
        return parse_pyproject_python_requires
    elif metadata_file.name == "setup.cfg":
        return parse_setupcfg_python_requires
    elif metadata_file.name == ".python-version":
        return parse_pyversion_python_requires
    else:
        raise NotImplementedError(f"Unknown metadata file type: {metadata_file.name}")


def parse_pyproject_python_requires(pyproject_file):
    #type: (pathlib.Path) -> typing.Optional[str]
    """Parse the project.requires-python field from a pyproject.toml file.

    Assumes that the pyproject.toml file exists, is accessible and well formatted.

    Returns None if the field is not found.
    """
    content = pyproject_file.read_text()
    pyproject = tomllib.loads(content)

    return pyproject.get("project", {}).get("requires-python", None)


def parse_setupcfg_python_requires(setupcfg_file):
    #type: (pathlib.Path) -> typing.Optional[str]
    """Parse the options.python_requires field from a setup.cfg file.

    Assumes that the setup.cfg file exists, is accessible and well formatted.

    Returns None if the field is not found.
    """
    config = configparser.ConfigParser()
    config.read(setupcfg_file)

    return config.get("options", "python_requires", fallback=None)


def parse_pyversion_python_requires(pyversion_file):
    #type: (pathlib.Path) -> typing.Optional[str]
    """Parse the python version from a .python-version file.

    Assumes that the .python-version file exists, is accessible and well formatted.

    Returns None if the field is not found.
    """
    return adapt_python_requires(pyversion_file.read_text().strip())


def adapt_python_requires(python_requires):
    #type: (str) -> str
    """Convert a literal python version to a PEP440 constraint.

    Connect expects a PEP440 format, but the .python-version file can contain
    plain version numbers and other formats.

    We should convert them to the constraints that connect expects.
    """
    current_contraints = python_requires.split(",")

    def _adapt_contraint(constraints):
        # type: (typing.List[str]) -> typing.Generator[str, None, None]
        for constraint in constraints:
            constraint = constraint.strip()
            if "@" in constraint or "-" in constraint or "/" in constraint:
                raise InvalidVersionConstraintError(f"python specific implementations are not supported: {constraint}")

            if "b" in constraint or "rc" in constraint or "a" in constraint:
                raise InvalidVersionConstraintError(f"pre-release versions are not supported: {constraint}")

            if re.match(VALID_VERSION_REQ_REGEX, constraint) is None:
                raise InvalidVersionConstraintError(f"Invalid python version: {constraint}")

            if re.search(PEP440_OPERATORS_REGEX, constraint):
                yield constraint
            else:
                # Convert to PEP440 format
                if "*" in constraint:
                    yield f"=={constraint}"
                else:
                    # only major specified “3” → ~=3.0 → >=3.0,<4.0
                    # major and minor specified “3.8” or “3.8.11” → ~=3.8.0 → >=3.8.0,<3.9.0
                    constraint = ".".join(constraint.split(".")[:2] + ["0"])
                    yield f"~={constraint}"

    return ",".join(_adapt_contraint(current_contraints))


class InvalidVersionConstraintError(ValueError):
    pass
