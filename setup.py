#! /usr/bin/env python
#
# Copyright (C) 2009, dmitry Vasiliev <dima@hlabs.spb.ru>
#

__author__ = "Dmitry Vasiliev <dima@hlabs.spb.ru>"

from setuptools import setup, find_packages


setup(
    name="erlport",
    version="0.0.2",
    description="Erlang port protocol",
    author="Dmitry Vasiliev",
    author_email="dima@hlabs.spb.ru",

    packages=find_packages("src", exclude=["*.tests"]),
    package_dir={"": "src"},

    zip_safe = True,
    test_suite="erlport.tests.test_suite",
)
