#!/usr/bin/env bash
# compress-images.sh — Near-lossless compression of images in the Bizard repository
#
# Uses pngquant + optipng for PNG, jpegoptim for JPEG
#
# Usage:
#   bash scripts/compress-images.sh [directory]

set -euo pipefail

DIR="${1:-images}"
QUALITY="80-95"

echo "=== Bizard Image Compression ==="
echo "Directory: $DIR"
echo ""

for tool in pngquant optipng; do
    if ! command -v "$tool" &>/dev/null; then
        echo "Installing $tool..."
        sudo apt-get update -qq && sudo apt-get install -y -qq "$tool"
    fi
done
if ! command -v jpegoptim &>/dev/null; then
    echo "Installing jpegoptim..."
    sudo apt-get update -qq && sudo apt-get install -y -qq jpegoptim
fi

total_before=0
total_after=0
count=0

compress_png() {
    local file="$1"
    local before
    before=$(stat -c%s "$file" 2>/dev/null || stat -f%z "$file")
    [ "$before" -lt 1024 ] && return
    pngquant --quality="$QUALITY" --speed 1 --force --output "$file" -- "$file" 2>/dev/null || true
    optipng -quiet -o2 "$file" 2>/dev/null || true
    local after
    after=$(stat -c%s "$file" 2>/dev/null || stat -f%z "$file")
    local saved=$((before - after))
    if [ "$saved" -gt 0 ]; then
        local pct=$((saved * 100 / before))
        echo "  ✓ $(basename "$file"): $(numfmt --to=iec "$before") → $(numfmt --to=iec "$after") (-${pct}%)"
        count=$((count + 1))
    fi
    total_before=$((total_before + before))
    total_after=$((total_after + after))
}

compress_jpg() {
    local file="$1"
    local before
    before=$(stat -c%s "$file" 2>/dev/null || stat -f%z "$file")
    [ "$before" -lt 1024 ] && return
    jpegoptim --strip-all --max=90 --quiet "$file" 2>/dev/null || true
    local after
    after=$(stat -c%s "$file" 2>/dev/null || stat -f%z "$file")
    local saved=$((before - after))
    if [ "$saved" -gt 0 ]; then
        local pct=$((saved * 100 / before))
        echo "  ✓ $(basename "$file"): $(numfmt --to=iec "$before") → $(numfmt --to=iec "$after") (-${pct}%)"
        count=$((count + 1))
    fi
    total_before=$((total_before + before))
    total_after=$((total_after + after))
}

echo "Compressing PNG files..."
while IFS= read -r -d '' file; do
    compress_png "$file"
done < <(find "$DIR" -type f -iname '*.png' -print0)

echo ""
echo "Compressing JPEG files..."
while IFS= read -r -d '' file; do
    compress_jpg "$file"
done < <(find "$DIR" -type f \( -iname '*.jpg' -o -iname '*.jpeg' \) -print0)

echo ""
echo "=== Compression Summary ==="
echo "Files optimized: $count"
if [ "$total_before" -gt 0 ]; then
    total_saved=$((total_before - total_after))
    total_pct=$((total_saved * 100 / total_before))
    echo "Total before: $(numfmt --to=iec "$total_before")"
    echo "Total after:  $(numfmt --to=iec "$total_after")"
    echo "Total saved:  $(numfmt --to=iec "$total_saved") (-${total_pct}%)"
fi
