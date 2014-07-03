RDFLib plugin providing JSON-LD parsing and serialization
=========================================================

This parser/serializer will:

* read in an JSON-LD formatted document and create an RDF graph
* serialize an RDF graph to JSON-LD formatted output


Using the plug-in JSONLD serializer/parser with RDFLib
------------------------------------------------------

The plugin parser and serializer are automatically registered if installed by
setuptools.

    >>> from rdflib import Graph, plugin
    >>> from rdflib.serializer import Serializer

    >>> testrdf = '''
    ... @prefix dc: <http://purl.org/dc/terms/> .
    ... <http://example.org/about>
    ...     dc:title "Someone's Homepage"@en .
    ... '''

    >>> g = Graph().parse(data=testrdf, format='n3')

    >>> print(g.serialize(format='json-ld', indent=4))
    {
        "@id": "http://example.org/about",
        "http://purl.org/dc/terms/title": [
            {
                "@language": "en",
                "@value": "Someone's Homepage"
            }
        ]
    }

    >>> context = {"@vocab": "http://purl.org/dc/terms/", "@language": "en"}
    >>> print(g.serialize(format='json-ld', context=context, indent=4))
    {
        "@context": {
            "@language": "en",
            "@vocab": "http://purl.org/dc/terms/"
        },
        "@id": "http://example.org/about",
        "title": "Someone's Homepage"
    }


Building the Sphinx documentation
---------------------------------

If Sphinx is installed, Sphinx documentation can be generated with::

    $ python setup.py build_sphinx

The documentation will be created in ./build/sphinx.


Continuous integration tests
----------------------------

[![Build Status](https://travis-ci.org/RDFLib/rdflib-jsonld.png?branch=master)](https://travis-ci.org/RDFLib/rdflib-jsonld)
