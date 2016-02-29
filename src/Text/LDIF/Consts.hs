module Text.LDIF.Consts where

-- | Chars with special meaning in DN
specialDNChars :: [Char]
specialDNChars = [',','=','+','<','>','#',';','/']

-- | Chars necessary to be escaped in DN when they are part of value
escapedDNChars :: [Char]
escapedDNChars = ['\\', '"'] ++ specialDNChars
