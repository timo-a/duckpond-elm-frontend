# duckpond-elm-frontend
elm frontend for duck in the pond / the fox and the duck

# build:
```
git clone https://github.com/timo-a/duckpond-elm-frontend.git
cd duckpond-elm-frontend/
# generator.jar not included
java -jar openapi-generator-cli-5.*.jar generate -g elm -i pond.yaml -o generated
elm make src/Pondescape.elm --optimize
firefox index.html
```
