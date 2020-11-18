module Main where

import SimpleSQLEngine

db0 = [ ( "movie"
        , [ [ ( "id", "1" ), ( "name", "Avatar"   ), ( "directorID", "1" ) ]
--          , [ ( "id", "2" ), ( "name", "Titanic"  ), ( "directorID", "1" ) ]
--           , [ ( "id", "3" ), ( "name", "Infamous" ), ( "directorID", "2" ) ]
--           , [ ( "id", "4" ), ( "name", "Skyfall"  ), ( "directorID", "3" ) ]
--           , [ ( "id", "5" ), ( "name", "Aliens"   ), ( "directorID", "1" ) ]
          ]
        )
      , ( "actor"
        , [ [ ( "id", "1" ), ( "name", "Leonardo DiCaprio" ) ]
--          , [ ( "id", "2" ), ( "name", "Sigourney Weaver"  ) ]
          , [ ( "id", "3" ), ( "name", "Daniel Craig"      ) ]
          ]
        )
      , ( "director"
        , [ [ ( "id", "1" ), ( "name", "James Cameron"   ) ]
          , [ ( "id", "2" ), ( "name", "Douglas McGrath" ) ]
           , [ ( "id", "3" ), ( "name", "Sam Mendes"      ) ]
          ]
        )
      , ( "actor_to_movie"
        , [ [ ( "movieID", "1" ), ( "actorID", "2" ) ]
--          , [ ( "movieID", "2" ), ( "actorID", "1" ) ]
--          , [ ( "movieID", "3" ), ( "actorID", "2" ) ]
--          , [ ( "movieID", "3" ), ( "actorID", "3" ) ]
--          , [ ( "movieID", "4" ), ( "actorID", "3" ) ]
--          , [ ( "movieID", "5" ), ( "actorID", "2" ) ]
          ]
        )
      ]

sql = "select movie.name   \
                            \     , actor.name\
                            \  FROM movie\
                            \  Join actor_to_movie\
                            \       oN actor_to_movie.movieID=movie.id\
                            \  JoIn actor\
                            \    oN actor_to_movie.actorID = actor.id\
                            \ WheRe \n\
                            \   actor.name <> 'Daniel Craig'"

main :: IO ()
main = do
  let execute = putStrLn . show . sqlEngine db0
  let parseast = putStrLn . show . parse
--  execute "select movie.name from movie"
--  execute "SELECT movie.name FROM movie WHERE movie.directorID = '1'"
--  execute "SelecT movie.name,director.name\nFroM director\nJoiN movie ON director.id = movie.directorID\n"
  parseast sql
  execute sql

