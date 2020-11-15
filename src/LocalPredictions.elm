
duckStep : Model -> Model
duckStep model = 
  let
    duckposN = toPolar <| on add fromPolar model.duckpos model.duckplan
  in
  { model | duckpos = duckposN }
