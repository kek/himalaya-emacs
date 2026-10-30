# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added `himalaya-log-level` option, passed to Himalaya CLI as `--log-level`.
- Added `himalaya-date-format` option to format envelope dates.
- Added `himalaya-list-envelopes-has-attachment` option to disable the attachment lookup.
- Added `himalaya-attachment-download-dir` option to override the download directory.
- Added `himalaya-message-headers` option listing the headers shown when reading a message.
- Added `C-c C-d` message writing keybind to save the message as a draft.
- Added buttons saving the attachments of a read message.

### Changed

- Adjusted API to match Himalaya CLI `v2`, which is now required.
- Renamed folders to mailboxes, following Himalaya CLI: `himalaya-folder` is now `himalaya-mailbox`, `himalaya-switch-folder` is now `himalaya-switch-mailbox` and `himalaya-expunge-folder` is now `himalaya-expunge-mailbox`. The old names are obsolete aliases. The file `himalaya-folder.el` is now `himalaya-mailbox.el`.
- Messages are rendered by Emacs from their raw MIME (`mm-decode`), HTML parts included, and composed with Emacs MML.
- Deleting messages moves them to the trash, as Himalaya CLI `message delete` does.
- Expunging uses the IMAP-specific API, the only one offering it.
- Flags are lowercase IANA names (`seen`, `answered`, `flagged`, `draft`). The deleted flag can no longer be added or removed.
- Next and previous message follow the envelope list.
- Errors show the message reported by Himalaya CLI.

### Fixed

- Fixed replying to and forwarding the message at point from the envelope list.
- Fixed expunging, which failed before asking for confirmation.

## [1.0] - 2023-02-09

### Added

- Added `Deleted` char in flags column.
- Added `himalaya-config-path` option to override the default configuration file path.
- Added `e` envelope listing keybind to expunge the current folder.
- Added `C-c +` envelope listing keybind to add a flag to marked envelopes (or envelope at point) [#22]. 
- Added `C-c -` envelope listing keybind to add a flag to marked envelopes (or envelope at point) [#22].
- Added preview support for `RET` envelope listing keybind using the universal argument, which allow you to preview a message without applying the "Seen" flag.

### Changed

- Adjusted API to match Himalaya CLI `v1.0.0`.
- Renamed option `himalaya-email-order` to `himalaya-list-envelopes-order`.
- Renamed option `himalaya-page-size` to `himalaya-list-envelopes-page-size`.
- Reduced default envelopes listing page size from `50` to `25`.
- Made all CLI calls async (except for envelopes listing due to table issue).
- Moved code into domain-specific files (account, folder, envelope, message, flag, template, attachment).
- Renamed functions containing `email` by either `envelope` or `message`, depending on the domain they belong to.

### Fixed

- Fixed `Answered` flag not set when sending an email from another folder than `INBOX`.

### Removed

- Removed option `himalaya-default-account`.
- Removed option `himalaya-default-folder`.

## [0.3] - 2023-02-09

The [0.2] has been reverted due to unintentional early merge, so the [0.3] is just a bump to avoid conflicts.

## [0.2] - 2023-01-09

### Added

- Included code from a [fork](https://git.sr.ht/~soywod/himalaya-emacs) [#15].
- Added possibility to mark multiple emails. Compatible actions: attachments, copy, move and delete. The implementation was inspired by the [tablist](https://github.com/politza/tablist) package) [#17].

### Changed

- Adjusted code for Himalaya CLI [v0.7.0](https://github.com/soywod/himalaya/pull/433).

### Fixed

- Fixed `nil` subjects in read buffers.
- Fixed `Answered` flag not set after replying to an email.

## [0.1] - 2022-10-09

First release added to the [MELPA](https://github.com/melpa/melpa/pull/7952) repository.

[Unreleased]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.3...HEAD
[0.3]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.2...v0.3
[0.2]: https://github.com/dantecatalfamo/himalaya-emacs/compare/v0.1...v0.2
[0.1]: https://github.com/dantecatalfamo/himalaya-emacs/compare/init...v0.1

[#15]: https://github.com/dantecatalfamo/himalaya-emacs/pull/15
[#17]: https://github.com/dantecatalfamo/himalaya-emacs/pull/17
[#22]: https://github.com/dantecatalfamo/himalaya-emacs/issues/22
