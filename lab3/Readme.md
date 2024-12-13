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
для запуска испольняемого файла (если в `lab3.cabal` поменять `Server.hs` на `Main.hs`)
```bash
cabal run 
```
для запуска сервера
```bash
 cabal run lab3-server
```
для запроса на сервер с записью в файл `res.json`
```cmd
curl -X POST http://localhost:8080 -H "Content-Type: application/json" -d "{\"grammarIndex\":1,\"numTests\":3}" > res.json
```