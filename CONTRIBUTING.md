# Contributing to Transient

# Getting Started
* Create a [GitHub](https://github.com) account if you do not already have one
* Check if a ticket for your issue exists, if not create one
    * Make sure your ticket details the issue and the steps to reproduce the bug
    * If your ticket proposes a new feature for Transient, please provide an explanation of the feature, what problem it solves, and possible use cases
* Fork the repository on GitHub

# Changing Transient
* Create a branch from `master` to work on your feature with a descriptive name
* Make commits frequently with descriptive comments (detailed below)
* Add tests to ensure proper functionality
* Please do not submit until all tests are passed

Commit messages should stick to the following format: `(issue number) issue name description`

E.g:

```
Example issue
Steps to recreate:
- Put toast in oven
- Turn oven off
- ????

An issue would then here go into detail describing the issue, and perhaps even suggesting a fix
```

# Making Small Changes
When changing things like documentation, it is not always necessary to create a ticket. Instead simply add the documentation, and send a PR with a message of the following form:

```
(doc) Added documentation to <file-name>
<file-name> lacked proper documentation on how <function> works.

This commit adds documentation describing <function>, and provides various examples.
```

# Submitting Changes
* Push your changes to the branch in your fork of the repository
* Submit a pull request
* The Transient team will review your PR as quickly and provide feedback
* After receiving feedback, either make the required changes, or your branch will be merged

Thanks for contributing to Transient, happy hacking!
