import Text.ParserCombinators.ReadP


report = "BIRK 281500Z 09014KT CAVOK M03/M06 Q0980 R13/910195"

airport :: ReadP String
airport = do
    code <- many1 (satisfy (\char -> char >= 'A' && char <= 'Z'))
    string " "
    return code


digit :: ReadP Char
digit =
    satisfy (\char -> char >= '0' && char <= '9')

timestamp :: ReadP (Int, Int, Int) 
timestamp = do
    day <- count 2 digit
    hour <- count 2 digit
    minute <- count 2 digit
    string "Z "
    return (read day, read hour, read minute)

