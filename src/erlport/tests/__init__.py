import doctest
import unittest


def test_suite():
    suite = unittest.TestSuite()
    suite.addTest(doctest.DocFileSuite("erlterms.txt"))
    suite.addTest(doctest.DocFileSuite("erlproto.txt"))
    return suite
