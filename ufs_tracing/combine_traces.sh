#!/bin/bash
set -eux

src=test

# Parse ufs.configure and calculate total PEs for each component
declare -A pe_counts
while IFS= read -r line; do
    if [[ $line =~ ^([A-Z]+)_petlist_bounds:[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
        component="${BASH_REMATCH[1]}"
        start="${BASH_REMATCH[2]}"
        end="${BASH_REMATCH[3]}"
        total=$((end - start + 1))
        pe_counts[$component]=$total
    fi
done < "${src}/ufs.configure"

# Check for MOM_override file and extract layout values
layout_suffix=""
io_layout_suffix=""
if [ -f "${src}/MOM_override" ]; then
    layout_x=$(grep -oP 'LAYOUT\s*=\s*\K\d+' "${src}/MOM_override" | head -1)
    layout_y=$(grep -oP 'LAYOUT\s*=\s*\d+\s*,\s*\K\d+' "${src}/MOM_override" | head -1)
    if [[ -n "$layout_x" && -n "$layout_y" ]]; then
        layout_suffix=".layout${layout_x}.${layout_y}"
    fi
    
    io_layout_x=$(grep -oP 'IO_LAYOUT\s*=\s*\K\d+' "${src}/MOM_override" | head -1)
    io_layout_y=$(grep -oP 'IO_LAYOUT\s*=\s*\d+\s*,\s*\K\d+' "${src}/MOM_override" | head -1)
    if [[ -n "$io_layout_x" && -n "$io_layout_y" ]]; then
        io_layout_suffix=".io${io_layout_x}.${io_layout_y}"
    fi
fi

# Build filename with PE counts
FILE="${src}"
for comp in ATM MED ICE OCN; do
    if [[ -n "${pe_counts[$comp]:-}" ]]; then
        FILE="${FILE}.$(echo $comp | tr '[:upper:]' '[:lower:]')${pe_counts[$comp]}"
        # Add layout suffix after OCN component if it exists
        if [[ "$comp" == "OCN" && -n "$layout_suffix" ]]; then
            FILE="${FILE}${layout_suffix}"
        fi
        # Add io_layout suffix after OCN component if it exists
        if [[ "$comp" == "OCN" && -n "$io_layout_suffix" ]]; then
            FILE="${FILE}${io_layout_suffix}"
        fi
    fi
done
FILE="${FILE}.trace"

if [ -f "$FILE" ]; then
    rm "$FILE"
fi

cat ${src}/ufs_trace_*.trace > all.traces
sed -i '$ s/.$//' all.traces
echo '[' > out.trace
cat all.traces >> out.trace
echo ']' >> out.trace
mv out.trace $FILE
