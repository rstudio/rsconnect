#!/usr/bin/env python
import datetime
import json
import locale
import os
import re
import subprocess
import sys

version_re = re.compile(r'\d+\.\d+(\.\d+)?')
exec_dir = os.path.dirname(sys.executable)


class EnvironmentException(Exception):
    pass


def detect_environment(dirname, force_generate=False, compatibility_mode=False, conda=None):
    """Determine the python dependencies in the environment.

    `pip freeze` will be used to introspect the environment.

    Returns a dictionary containing the package spec filename
    and contents if successful, or a dictionary containing 'error'
    on failure.
    :param: dirname Directory name
    :param: force_generate Force the generation of an environment
    :param: compatibility_mode Force the usage of `pip freeze` for older
    connect versions which do not support conda.
    """
    if not compatibility_mode:
        conda = get_conda(conda)
    if conda:
        if force_generate:
            result = conda_env_export(conda)
        else:
            result = (output_file(dirname, 'environment.yml', 'conda')
                      or conda_env_export(conda))
    else:
        if force_generate:
            result = pip_freeze()
        else:
            result = (output_file(dirname, 'requirements.txt', 'pip')
                      or pip_freeze())

    if result is not None:
        result['python'] = get_python_version()
        result['pip'] = get_version('pip')
        if conda:
            result['conda'] = get_conda_version(conda)
        result['locale'] = get_default_locale()

    return result


def get_conda(conda=None):
    """get_conda tries to find the conda executable if we're in
    a conda environment. If not, or if we cannot find the executable,
    return None.
    :returns: conda string path to conda or None.
    """
    if os.environ.get('CONDA_PREFIX', None) is None and conda is None:
        return None
    else:
        return conda or os.environ.get('CONDA_EXE', None)


def get_python_version():
    v = sys.version_info
    return "%d.%d.%d" % (v[0], v[1], v[2])


def get_conda_version(conda):
    try:
        args = [conda, '-V']
        proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = proc.communicate()
        match = version_re.search(stdout)
        if match:
            return match.group()
        msg = "Failed to get version of conda from the output of: %s" % (' '.join(args))
        raise EnvironmentException(msg)
    except Exception as exception:
        raise EnvironmentException("Error getting conda version: %s" % str(exception))


def get_default_locale(locale_source=locale.getdefaultlocale):
    result = '.'.join([item or '' for item in locale_source()])
    return '' if result == '.' else result


def get_version(module):
    try:
        args = [sys.executable, '-m', module, '--version']
        proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = proc.communicate()
        match = version_re.search(stdout)
        if match:
            return match.group()

        msg = "Failed to get version of '%s' from the output of: %s" % (module, ' '.join(args))
        raise EnvironmentException(msg)
    except Exception as exception:
        raise EnvironmentException("Error getting '%s' version: %s" % (module, str(exception)))


def output_file(dirname, filename, package_manager):
    """Read an existing package spec file.

    Returns a dictionary containing the filename and contents
    if successful, None if the file does not exist,
    or a dictionary containing 'error' on failure.
    """
    try:
        path = os.path.join(dirname, filename)
        if not os.path.exists(path):
            return None

        with open(path, 'r') as f:
            data = f.read()

        data = '\n'.join([line for line in data.split('\n')
                          if 'rsconnect' not in line])

        return {
            'filename': filename,
            'contents': data,
            'source': 'file',
            'package_manager': package_manager,
        }
    except Exception as exception:
        raise EnvironmentException('Error reading %s: %s' % (filename, str(exception)))


def pip_freeze():
    """Inspect the environment using `pip freeze`.

    Returns a dictionary containing the filename
    (always 'requirements.txt') and contents if successful,
    or a dictionary containing 'error' on failure.
    """
    try:
        proc = subprocess.Popen(
            [sys.executable, '-m', 'pip', 'freeze'],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)

        pip_stdout, pip_stderr = proc.communicate()
        pip_status = proc.returncode
    except Exception as exception:
        raise EnvironmentException('Error during pip freeze: %s' % str(exception))

    if pip_status != 0:
        msg = pip_stderr or ('exited with code %d' % pip_status)
        raise EnvironmentException('Error during pip freeze: %s' % msg)

    pip_stdout = '\n'.join([line for line in pip_stdout.split('\n')
                            if 'rsconnect' not in line])

    pip_stdout = '# requirements.txt generated by rsconnect-python on '+str(datetime.datetime.utcnow())+'\n'+pip_stdout

    return {
        'filename': 'requirements.txt',
        'contents': pip_stdout,
        'source': 'pip_freeze',
        'package_manager': 'pip',
    }


def conda_env_export(conda):
    """Inspect the environment using `conda env export`
    :param: conda path to the `conda` tool
    :return: dictionary containing the key "environment.yml" and the data inside
    """
    try:
        proc = subprocess.Popen(
            [conda, 'env', 'export'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        conda_stdout, conda_stderr = proc.communicate()
        conda_status = proc.returncode
    except Exception as exception:
        raise EnvironmentException('Error during conda env export: %s' % str(exception))

    if conda_status != 0:
        msg = conda_stderr or ('exited with code %d' % conda_status)
        raise EnvironmentException('Error during conda env export: %s' % msg)

    return {
        'filename': 'environment.yml',
        'contents': conda_stdout,
        'source': 'conda_env_export',
        'package_manager': 'conda'
    }


def main():
    try:
        if len(sys.argv) < 2:
            raise EnvironmentException('Usage: %s [-fc] DIRECTORY' % sys.argv[0])
        # directory is always the last argument
        directory = sys.argv[len(sys.argv)-1]
        flags = ''
        force_generate = False
        compatibility_mode = False
        if len(sys.argv) > 2:
            flags = sys.argv[1]
        if 'f' in flags:
            force_generate = True
        if 'c' in flags:
            compatibility_mode = True
        result = detect_environment(directory, force_generate, compatibility_mode)
    except EnvironmentException as exception:
        result = dict(error=str(exception))

    json.dump(result, sys.stdout, indent=4)


if __name__ == '__main__':
    main()
