# Changelog

All notable changes to this project will be documented in this file.
The changelog format adheres to [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Package Versioning Policy](http://pvp.haskell.org/).

## 0.1.4.0 [2016-08-11]

### Added
- A new `--url` (or `-u`) option to specify the url of the Hue to talk to. The `FHUE_URL` environment variable can also be used for that.

## 0.1.3.0 [2016-08-01]

### Added
- Integration tests! Hopefully, that should mean no more regressions!
- All paths on HDFS can now be relative to `/user/USERNAME`.
- `ls` can be called without arguments: it lists files in `/user/USERNAME`.
- `put` can be called only with the file to upload: it will be uploaded to `/user/USERNAME`.
- FHue will now ask for your login if the password doesn't work any more.

### Changed
- Make less requests by reusing the CSRF token accross requests. This should make FHue faster :)

## 0.1.2.0 [2016-07-22]

### Added
- `rm` command (you know what is does).
- `edit` command to edit a file on HDFS.

### Changed
- `ls` now displays up to 1000 items, and prints a warning if there are more

### Fixed
- It should now work for the rest of the world

## 0.1.1.1 [2016-07-22]

### Fixed
- No more failure with empty error messages.

## 0.1.1.0 [2016-07-21]

### Added
- A new `get` command to download files!

### Fixed
- put: temporary file for upload is now in destination dir and not /tmp

## 0.1.0.0 [2016-07-13]
Initial release
