# setup.py
from distutils.core import setup
import sys
import myro

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
    description="My Robot Python Exploration Library",
    version= myro.__VERSION__,
    author="Doug Blank",
    author_email="dblank@cs.brynmawr.edu",
    url="http://www.roboteducation.org/",
    packages=['myro', 'myro.robot', 'myro.worlds', 'myro.globals'],
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
