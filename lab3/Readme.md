```bash
cabal clean
```
```bash
cabal build
```
для тестов
```bash
cabal test
```
для запуска испольняемого файла 
```bash
cabal run fuzzModule
```
для запуска сервера
```bash
 cabal run lab3-server
```
для запроса на сервер с записью в файл `res.json`\
для выбора тестовой грамматики
```cmd
curl -X POST http://localhost:8080 -H "Content-Type: application/json" -d "{\"grammarIndex\":1,\"numTests\":3}" > res.json
```
для ввода кастомной грамматики
```cmd
curl -X POST http://localhost:8080 -H "Content-Type: application/json" -d "{\"grammarIndex\":null,\"numTests\":5,\"customGrammar\":{\"grammarRules\":[{\"lhs\":\"S\",\"rhs\":[\"a\",\"S\",\"b\"]},{\"lhs\":\"S\",\"rhs\":[\"ε\"]}]}}" > res_custom.json
```

```json
{
  "grammarIndex": null,
  "numTests": 5,
  "customGrammar": {
    "grammarRules": [
      {
        "lhs": "S",
        "rhs": ["a", "S", "b"]
      },
      {
        "lhs": "S",
        "rhs": ["ε"]
      }
    ]
  }
}
```