# Fhue

FHue talks to HDFS through Hue, so you can use HDFS without direct access.

## Install

- Go to the latest release: https://github.com/madjar/fhue/releases/latest
- Download the binary for your system (`fhue-linux` or `fhue-mac`)
- Rename it to `fhue` and put it somewhere nice (`mv ~/Downloads/fhue-mac ~/.local/bin`)
- Make it executable (`chmod +x ~/.local/binfhue`)
- Enjoy!

## Usage

This is a command line tools with a few subcommands. Call each subcommand to get more help.

```
FHue

Usage: fhue [--version] [--help] COMMAND
  A tool to interact with hdfs through Hue

Available options:
  --version                Show version
  --help                   Show this help text

Available commands:
  ls                       List a directory
  put                      Upload a file
  get                      Download a file to target directory
```

