LDIF files parser implementation using Parsec.
The LDAP Data Interchange Format (LDIF) is defined by RFC 2849.

Current implementation is not complete and compliant with RFC.

Package includes following command line tools:

- ldifdiff - calculates delta LDIF between two content LDIF files.
- ldif2html - produces HTML/browsable LDIF file.
- ldifmodify - replays delta LDIF operations on content LDIF (similar to ldapmodify).
- ldifundo - produces delta LDIF which rollbacks operations in input LDIF.

Originally from https://hackage.haskell.org/package/ldif.

The original source repository is no longer accessible so the revision history
is imported from the source tarballs from Hackage.
