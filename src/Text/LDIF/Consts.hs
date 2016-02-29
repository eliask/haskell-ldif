module Text.LDIF.Consts where

-- | Chars necessary to be escaped when are within RDN values
specialDNChars, escapedDNChars :: [Char]
specialDNChars = [',','=','+','<','>','#',';']
escapedDNChars = ['\\', '"'] ++ specialDNChars
