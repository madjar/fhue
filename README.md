# Fhue

FHue talks to HDFS through Hue, so you can use HDFS without direct access.

## Install

- Go to the latest release: https://github.com/madjar/fhue/releases/latest
- Download the binary for your system (`fhue-linux` or `fhue-mac`)
- Rename it to `fhue` and put it somewhere nice (`mv ~/Downloads/fhue-mac ~/.local/bin`)
- Make it executable (`chmod +x ~/.local/binfhue`)
- Enjoy!

## Usage

You'll need to tell FHue the url of the Hue you want to talk to. For that, you
can use the `--url` or `-u` option, or the `FHUE_URL` environment variable.

- If you always talk to the same Hue, add `export FHUE_URL=https://path.to.hue` to your `~/.bashrc` (or run `set -Ux FHUE_URL https://path.to.hue` in fish).
- If you use different Hue instance, you could use aliases. For example `alias fud=fhue --url https://hue.dev` and `alias fup=fhue --url https://hue.prod`.

This is a command line tools with a few subcommands. Call each subcommand to get more help.

```
FHue

Usage: fhue [--version] [--help] [-u|--url HUE_URL] COMMAND
  A tool to interact with hdfs through Hue

Available options:
  --version                Show version
  --help                   Show this help text
  -u,--url HUE_URL         Url of the Hue server to talk to. Defaults to the
                           FHUE_URL environment variable (default: "tt")

Available commands:
  ls                       List a directory
  put                      Upload a file
  get                      Download a file to target directory
  rm                       Delete a file or directory
  edit                     Edit a file with your $EDITOR
```

