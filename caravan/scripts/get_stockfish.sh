#!/bin/sh
set -eu

version=19
script_dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
destination="$script_dir/../bin/stockfish"
os=$(uname -s)
arch=$(uname -m)

case "$os:$arch" in
    Darwin:arm64|Darwin:x86_64) platform=macos-universal ;;
    Linux:x86_64) platform=linux-x86-64-universal ;;
    Linux:aarch64|Linux:arm64) platform=linux-arm64-universal ;;
    Linux:riscv64) platform=linux-riscv64-universal ;;
    MINGW*:x86_64|MSYS*:x86_64|CYGWIN*:x86_64) platform=windows-x86-64-universal ;;
    MINGW*:aarch64|MSYS*:aarch64|CYGWIN*:aarch64|MINGW*:arm64|MSYS*:arm64|CYGWIN*:arm64)
        platform=windows-arm64-universal ;;
    *) printf 'Unsupported platform: %s %s\n' "$os" "$arch" >&2; exit 1 ;;
esac

extension=tar.gz
executable=stockfish
case "$platform" in
    windows-*) extension=zip; executable=stockfish.exe ;;
esac
asset="stockfish-$platform.$extension"
url="https://github.com/official-stockfish/Stockfish/releases/download/sf_$version/$asset"

case "$platform" in
    macos-universal) checksum=a1f0e3bcc5a6927a11fe6fc8e54a779754645f3c2bae2cf13420fd1957adaa77 ;;
    linux-x86-64-universal) checksum=9defc0d4e55d49c65a6d042f3e571a39fcea499ade6dbe741b53b8c65e03611f ;;
    linux-arm64-universal) checksum=fe26cfd1d9db4c8af3d21e24d9ff34cacb31c1f940085a7583da11796f2bac01 ;;
    linux-riscv64-universal) checksum=d4b4d118c003bc669a939d9fb208e3ef6b367730fd79a70dbc278b18475df3d1 ;;
    windows-x86-64-universal) checksum=3c8bf1f9ea66a09350a40df4f632288285ac206d99f33ab5842c408fc30b48a7 ;;
    windows-arm64-universal) checksum=8372ad3f0d7276deb2c70f801f541ec7db463219fc6d9c7592864e542aa4f401 ;;
esac

for tool in curl mktemp; do
    command -v "$tool" >/dev/null 2>&1 || {
        printf 'Required command not found: %s\n' "$tool" >&2
        exit 1
    }
done
if command -v sha256sum >/dev/null 2>&1; then
    hash_command=sha256sum
elif command -v shasum >/dev/null 2>&1; then
    hash_command=shasum
else
    printf 'Required command not found: sha256sum or shasum\n' >&2
    exit 1
fi

work_dir=$(mktemp -d "$script_dir/../bin/.stockfish-download.XXXXXX")
trap 'rm -rf -- "$work_dir"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

printf 'Downloading Stockfish %s (%s)...\n' "$version" "$platform"
curl --fail --location --retry 3 --output "$work_dir/$asset" "$url"
if [ "$hash_command" = sha256sum ]; then
    actual=$(sha256sum "$work_dir/$asset")
else
    actual=$(shasum -a 256 "$work_dir/$asset")
fi
if [ "${actual%% *}" != "$checksum" ]; then
    printf 'Stockfish archive checksum mismatch.\n' >&2
    exit 1
fi

case "$extension" in
    zip) unzip -q "$work_dir/$asset" -d "$work_dir" ;;
    tar.gz) tar -xzf "$work_dir/$asset" -C "$work_dir" ;;
esac
mv "$work_dir/stockfish/stockfish-$platform${executable#stockfish}" \
    "$work_dir/stockfish/$executable"
chmod +x "$work_dir/stockfish/$executable"
rm -rf -- "$destination"
mv "$work_dir/stockfish" "$destination"
printf 'Installed Stockfish %s: %s/%s\n' "$version" "$destination" "$executable"
