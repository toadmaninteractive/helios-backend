#!/usr/bin/env python3

import sys
import os
import json
import gzip
import shutil
import hashlib


def md5_file(file_path):
    chunk_size = 2**24 # 16 MB
    md5 = hashlib.md5()

    with open(file_path, 'rb') as file:
        while True:
            chunk = file.read(chunk_size)

            if not chunk:
                break

            md5.update(chunk)

    return md5.hexdigest()


def prepare(path_in, game_guid, branch, build_rev, cdn_root_url, path_out):
    tmp_files_to_del = []

    try:
        build_file_list = []
        total_build_size = 0
        total_gz_size = 0

        # Create output directory if not exists
        if not os.path.exists(path_out):
            os.makedirs(path_out)

        # Create files subdirectory if not exists
        path_out_files = os.path.join(path_out, 'files')

        # Clean files subdirectory recursively
        if os.path.exists(path_out_files):
            shutil.rmtree(path_out_files)

        os.makedirs(path_out_files)

        # Load build manifest
        build_manifest_path = os.path.join(path_in, 'manifest.json')

        with open(build_manifest_path, encoding='utf-8') as df:
            input_manifest = json.load(df)

        # Read ignored filenames and extensions
        files = input_manifest.get('files', {})
        ignored_files = [f.lower() for f in files.get('ignoredFiles', [])]
        ignored_exts = [e.lower() for e in files.get('ignoredExts', [])]

        # Loop thru files
        for rootdir, dirs, files in os.walk(path_in):
            for file in files:
                # Get file absolute and relative paths
                file_path = os.path.join(rootdir, file)
                relative_path = os.path.relpath(file_path, path_in)

                # Skip ignored files
                if os.path.basename(file).lower() in ignored_files:
                    print('Ignored by filename:', file_path)
                    continue

                # Skip ignored extensions
                if os.path.splitext(file)[1].lower() in ignored_exts:
                    print('Ignored by extension:', file_path)
                    continue

                # Skip input manifest
                if file_path == build_manifest_path:
                    print('Ignored input manifest:', file_path)
                    continue

                # Define output file path
                output_file_path = os.path.join(path_out_files, relative_path) + '.gz'

                # Create target output path
                os.makedirs(os.path.dirname(output_file_path), exist_ok=True)

                # Compress input file
                with open(file_path, 'rb') as f_in:
                    with gzip.open(output_file_path, 'wb', compresslevel=6) as f_out:
                        shutil.copyfileobj(f_in, f_out)

                # Update file size counters
                size = os.path.getsize(file_path)
                total_build_size += size

                gz_size = os.path.getsize(output_file_path)
                total_gz_size += gz_size

                # Update files to delete
                tmp_files_to_del.append(output_file_path)

                # Calculate file MD5 and append it to list
                build_file_list.append({
                    'md5': md5_file(file_path),
                    'relative_path': relative_path,
                    'relative_compressed_path': relative_path + '.gz',
                    'size': size,
                    'compressed_size': gz_size
                })

        # Redistributables: list of { "name": string, "url": string }

        # Create output manifest (to push to Helios)
        output_manifest_json = json.dumps({
            'guid': game_guid,
            'branch': branch,
            'build': build_rev,
            'cdn_root_url': '{}/{}/{}/'.format(cdn_root_url, game_guid, build_rev),
            'total_build_size': total_build_size,
            'total_compressed_size': total_gz_size,
            'exe_path': input_manifest['game']['exePath'],
            'log_path': input_manifest['game'].get('logPath') or '',
            'config_path': input_manifest['game'].get('configPath') or '',
            'crash_report_path': input_manifest['game'].get('crashReportPath') or '',
            'optional_file_masks': input_manifest['game'].get('optionalFileMasks') or [],
            'preserved_file_masks': input_manifest['game'].get('preservedFileMasks') or [],
            'redistributables': input_manifest.get('redistributables') or [],
            'pdb_files': files.get('pdbFiles') or [],
            'files': build_file_list
        }, indent=4)

        manifest = open(os.path.join(path_out, 'helios-manifest.json'), 'w')
        manifest.write(output_manifest_json)
        manifest.close()

    except Exception as e:
        print('EXCEPTION: ', e)
        print('ERROR')

        for file in tmp_files_to_del:
            os.remove(file)

        sys.exit(1)

    print('ok')
    sys.exit(0)


# argv[1] :: input directory
# argv[2] :: game GUID
# argv[3] :: branch
# argv[4] :: build-rev
# argv[5] :: CDN root URL prefix
# argv[6] :: output directory

prepare(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5], sys.argv[6])
