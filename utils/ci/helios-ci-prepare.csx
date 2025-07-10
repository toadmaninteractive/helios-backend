using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Json;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

if (Args.Count < 6)
{
    Console.WriteLine("Usage: helios-ci-prepare.csx source_path game_guid branch build_rev cdn_root_url target_path");
    Environment.Exit(1);
}

var sourcePath = Args[0];
var gameGuid = Args[1];
var branch = Args[2];
var buildRev = Args[3];
var cdnRootUrl = Args[4];
var targetPath = Args[5];

const string HeliosApiKey = "CHANGE_ME";

var stopwatch = new Stopwatch();
stopwatch.Start();

Directory.CreateDirectory(targetPath);
var pathOutFiles = Path.Join(targetPath, "files");
if (Directory.Exists(pathOutFiles))
    Directory.Delete(pathOutFiles, true);
Directory.CreateDirectory(pathOutFiles);

record ManifestFiles(string[] ignoredExts, string[] ignoredFiles, string[] pdbFiles);
record ManifestGame(string exePath, string logPath, string configPath, string crashReportPath, string[] preservedFileMasks, string[] optionalFileMasks);
record ManifestRedist(string name, string url);
record Manifest(ManifestGame game, ManifestFiles files, ManifestRedist[] redistributables);

var inputManifestPath = Path.Join(sourcePath, "manifest.json");
var inputManifest = JsonSerializer.Deserialize<Manifest>(File.ReadAllText(inputManifestPath));

var ignoredFiles = inputManifest.files.ignoredFiles.ToHashSet(StringComparer.OrdinalIgnoreCase);
var ignoredExts = inputManifest.files.ignoredExts.ToHashSet(StringComparer.OrdinalIgnoreCase);

bool IsIgnoredFile(string fileName)
{
    if (ignoredFiles.Contains(Path.GetFileName(fileName)))
    {
        Console.WriteLine($"Ignored by filename: {fileName}");
        return true;
    }

    if (ignoredExts.Contains(Path.GetExtension(fileName)))
    {
        Console.WriteLine($"Ignored by extension: {fileName}");
        return true;
    }

    if (string.Compare(fileName, "manifest.json", StringComparison.OrdinalIgnoreCase) == 0)
    {
        Console.WriteLine($"Ignored input manifest: {fileName}");
        return true;
    }

    return false;
}

static string FileMD5(FileStream sourceFile)
{
    using (var md5 = MD5.Create())
    {
        var hash = md5.ComputeHash(sourceFile);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }
}

record BuildFile(string md5, string relative_path, string relative_compressed_path, long size, long compressed_size);
record BuildManifest(List<BuildFile> files, string build_rev);

var httpClient = new HttpClient();
var oldBuildFiles = new Dictionary<string, BuildFile>();
string oldBuild = null;
try
{
    httpClient.DefaultRequestHeaders.Add("x-api-key", HeliosApiKey);
    var oldManifest = await httpClient.GetFromJsonAsync<BuildManifest>($"https://helios.yourcompany.com/api/manifest/{gameGuid}/{branch}", default);
    oldBuildFiles = oldManifest.files.ToDictionary(f => f.relative_path, f => f);
    oldBuild = oldManifest.build_rev;
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Warning: Failed to fetch build diff. Full build upload is required. Reason: {ex}");
}

(BuildFile file, bool keepOldFile) PrepareFile(string sourceFullPath)
{
    var sourceRelativePath = Path.GetRelativePath(sourcePath, sourceFullPath);
    if (IsIgnoredFile(sourceRelativePath))
        return (null, false);

    using FileStream sourceFileStream = File.OpenRead(sourceFullPath);
    var md5 = FileMD5(sourceFileStream);
    if (oldBuild != null && oldBuildFiles.TryGetValue(sourceRelativePath, out var oldBuildFile) && oldBuildFile.md5 == md5)
        return (oldBuildFile, true);

    var targetRelativePath = sourceRelativePath + ".gz";
    var outputFilePath = Path.Join(pathOutFiles, targetRelativePath);
    Directory.CreateDirectory(Path.GetDirectoryName(outputFilePath));
    sourceFileStream.Seek(0, SeekOrigin.Begin);
    using (FileStream compressedFileStream = File.Create(outputFilePath))
    {
        using GZipStream compressionStream = new GZipStream(compressedFileStream, CompressionMode.Compress);
        sourceFileStream.CopyTo(compressionStream);
    }
    var size = sourceFileStream.Length;
    var compressedSize = new FileInfo(outputFilePath).Length;
    return (new BuildFile(md5, sourceRelativePath, targetRelativePath, size, compressedSize), false);
}

var buildFileList = Directory.EnumerateFiles(sourcePath, "*.*", SearchOption.AllDirectories)
    .AsParallel()
    .Select(PrepareFile)
    .Where(f => f.file != null)
    .ToList();

// Create output manifest (to push to Helios)
var outputManifest = new
{
    guid = gameGuid,
    branch = branch,
    build = buildRev,
    cdn_root_url = string.Format($"{cdnRootUrl}/{gameGuid}/{buildRev}/"),
    total_build_size = buildFileList.Sum(f => f.file.size),
    total_compressed_size = buildFileList.Sum(f => f.file.compressed_size),
    exe_path = inputManifest.game.exePath ?? "",
    log_path = inputManifest.game.logPath ?? "",
    config_path = inputManifest.game.configPath ?? "",
    crash_report_path = inputManifest.game.crashReportPath ?? "",
    optional_file_masks = inputManifest.game.optionalFileMasks ?? Array.Empty<string>(),
    preserved_file_masks = inputManifest.game.preservedFileMasks ?? Array.Empty<string>(),
    redistributables = inputManifest.redistributables,
    pdb_files = inputManifest.files.pdbFiles ?? Array.Empty<string>(),
    files = buildFileList.Select(f => f.file).ToList(),
};

var outputManifestString = JsonSerializer.Serialize(outputManifest, new JsonSerializerOptions { WriteIndented = true });
File.WriteAllText(Path.Join(targetPath, "helios-manifest.json"), outputManifestString);

var script = new StringBuilder();
script.Append("#!/bin/bash\n");
var baseDirs = new HashSet<string>();
foreach ((var file, var keepOldFile) in buildFileList)
{
    if (keepOldFile)
    {
        var baseDir = Path.GetDirectoryName(file.relative_compressed_path);
        if (!baseDirs.Contains(baseDir))
        {
            baseDirs.Add(baseDir);
            script.Append($"mkdir -p {buildRev}/{baseDir.Replace('\\', '/')}\n");
        }
        var path = file.relative_compressed_path.Replace('\\', '/');
        script.Append($"ln ./{oldBuild}/{path} {buildRev}/{path}\n");
    }
}
File.WriteAllText(Path.Join(targetPath, "link_kept_files.sh"), script.ToString());

stopwatch.Stop();
Console.WriteLine($"Elapsed time: {stopwatch.Elapsed.TotalSeconds} seconds");
Console.WriteLine("OK");
