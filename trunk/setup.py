# setup.py

from distutils.core import setup
from distutils.file_util import copy_file
import sys
from myro import __VERSION__

#windows installer:
# python setup.py bdist_wininst

# patch distutils if it can't cope with the "classifiers" or
# "download_url" keywords

if sys.version < '2.2.3':
    from distutils.dist import DistributionMetadata
    DistributionMetadata.classifiers = None
    DistributionMetadata.download_url = None

setup(
    name="myro",
    description="My Robot Python Exploration Library, from the IPRE",
    version= __VERSION__,
    author="Doug Blank, and the IPRE",
    author_email="dblank@cs.brynmawr.edu",
    url="http://myro.roboteducation.org/",
    packages=['myro', 'myro.robot', 'myro.worlds', 'myro.globvars'],
    package_dir={'idlelib': 'misc'},
    package_data={'idlelib': ['config-extensions.def']},
    license="Shared Source",
    long_description="Tools for exploring robotics in education",
    classifiers = [
        'Development Status :: 5 - Production/Stable',
        'Intended Audience :: End Users/Desktop',
        'License :: Shared Source',
        'Natural Language :: English',
        'Operating System :: Microsoft :: Windows :: Mac',
        'Programming Language :: Python',
        'Topic :: Software Development :: Libraries',
    ],
)

import idlelib, os
path = idlelib.__file__.split(os.sep)[:-1]
path = os.sep.join(path)
copy_file("misc/ScriptBinding.py", path)
copy_file("misc/config-extensions.def", path)
