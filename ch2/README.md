# Ammonite
- `amm` to launch ammonite REPL
- scala script = file ending in `.sc`
- can run scripts like `amm myScript.sc`
- watch a script with `amm -w myScript.sc`
- load a script into the REPL like `amm --predef myScript.sc`

# Mill
- A build tool for Scala projects
- Compile example project with `./mill foo.compile`
- Compile and run project with `./mill foo.run`
- Mill script file can be copied and used in another project
  - Download the startup script with
```
curl -L https://github.com/lihaoyi/mill/releases/download/0.8.0/0.8.0 -o mill
chmod +x mill
```
- Run unit tests with `./mill foo.test`
- Create executable: `./mill foo.assembly`
- Show where executable will be output to: `./mill show foo.assembly`