# list-albums

Listing albums by album duration.

Documented in https://kisaragi-hiu.com/sort-albums-by-duration

Since no music player that I've tried supports this, I had to implement this myself. It's been useful from time to time, so I've made it a package.

## Requirements

- ffprobe (part of ffmpeg)
- Assumes music in the music dir is arranged as a flat list of albums, each album being a flat list of music files.

## Usage

Default variables values should be fine.

User options:

- `list-albums-cache-file`: a JSON object mapping album folder names to durations. Extra entries can also be added, which will also be listed; that's useful for albums on streaming platforms.
- `list-albums-music-dir`: Path to the music directory for discovering albums.

Command:

- `list-albums`: entry point, lists album names and their durations; provides sort by duration.
