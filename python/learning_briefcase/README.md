# `learning_briefcase`

Python tool for packaging applications into standalone, cross-platform installers.
  
See [beeware.org/project/projects/tools/briefcase](https://beeware.org/project/projects/tools/briefcase/).
  
## Usage

First run.

```console
$ pip install toga briefcase
$ briefcase new
```

Then replace the code within `src/learning_briefcase_app/__main__.py` with code within `app.py`.

Then run.

```console
$ briefcase build
$ briefcase run
$ briefcase package
```

Packaged app can be found within the `dist/` directory and is ready for distribution.

## Why use Briefcase?

* Cross-platform packaging of Python apps for Windows, macOS, Linux, iOS, and Android that allows for the *WRITE ONCE DEPLOY ANYWHERE* mindset.
* Native installers for standalone apps that don't require users to build the Python app from source by installing Python first.
* Simple configuration management via `pyproject.toml`.
* First-class integration with [Toga](https://beeware.org/project/projects/libraries/toga/) for building GUI applications in Python.
* Seamless support for the existing BeeWare ecosystem.