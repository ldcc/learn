module Main where

import G964Partition

db0 = [ ( "movie"
        , [ [ ( "id", "01" ), ( "title", "The A-Team"                                        ), ( "year", "2010" ), ( "directorID", "01" ) ]
          , [ ( "id", "02" ), ( "title", "Avatar"                                            ), ( "year", "2009" ), ( "directorID", "02" ) ]
          , [ ( "id", "03" ), ( "title", "Titanic"                                           ), ( "year", "1997" ), ( "directorID", "02" ) ]
          , [ ( "id", "04" ), ( "title", "The Avengers"                                      ), ( "year", "2012" ), ( "directorID", "03" ) ]
          , [ ( "id", "05" ), ( "title", "Iron Man 3"                                        ), ( "year", "2013" ), ( "directorID", "04" ) ]
          , [ ( "id", "06" ), ( "title", "Iron Man"                                          ), ( "year", "2008" ), ( "directorID", "05" ) ]
          , [ ( "id", "07" ), ( "title", "The Lord of the Rings: The Return of the King"     ), ( "year", "2003" ), ( "directorID", "06" ) ]
          , [ ( "id", "08" ), ( "title", "The Lord of the Rings: The Fellowship of the Ring" ), ( "year", "2001" ), ( "directorID", "06" ) ]
          , [ ( "id", "09" ), ( "title", "The Lord of the Rings: The Two Towers"             ), ( "year", "2002" ), ( "directorID", "06" ) ]
          , [ ( "id", "10" ), ( "title", "Skyfall"                                           ), ( "year", "2012" ), ( "directorID", "07" ) ]
          , [ ( "id", "11" ), ( "title", "The Dark Knight Rises"                             ), ( "year", "2012" ), ( "directorID", "08" ) ]
          , [ ( "id", "12" ), ( "title", "The Dark Knight"                                   ), ( "year", "2008" ), ( "directorID", "08" ) ]
          , [ ( "id", "13" ), ( "title", "Pirates of the Caribbean: Dead Man's Chest"        ), ( "year", "2006" ), ( "directorID", "09" ) ]
          , [ ( "id", "14" ), ( "title", "Toy Story 3"                                       ), ( "year", "2010" ), ( "directorID", "10" ) ]
          , [ ( "id", "15" ), ( "title", "E.T. the Extra-Terrestrial"                        ), ( "year", "1982" ), ( "directorID", "11" ) ]
          , [ ( "id", "16" ), ( "title", "Toy Story"                                         ), ( "year", "1995" ), ( "directorID", "12" ) ]
          , [ ( "id", "17" ), ( "title", "Pirates of the Caribbean: On Stranger Tides"       ), ( "year", "2011" ), ( "directorID", "13" ) ]
          , [ ( "id", "18" ), ( "title", "Jurassic Park"                                     ), ( "year", "1993" ), ( "directorID", "11" ) ]
          ]
        )
      ]

main :: IO ()
main = do
  let e = enum 5
  show s


isLochNessMonster xs@('t':'r':'e':'e':' ':'f':'i':'d':'d':'y':_) = f . words . map toLower