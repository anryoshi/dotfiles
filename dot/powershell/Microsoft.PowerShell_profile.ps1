# vim: set foldmethod=marker foldlevel=0 nomodeline:
# Anton Rybakov PowerShell profile file
# Intended to be used with PS >= 7
# External dependencies:
#     vswhere  - for determine VS location
#     starship - as cross-platform prompt

Set-PSReadLineOption -EditMode Emacs

function Enter-DevEnv {
    if (!(Get-Command vswhere -ErrorAction 'SilentlyContinue')) {
        throw "vswhere was not found"
    }

    $installedVSes = vswhere -prerelease -format json | ConvertFrom-Json
    if ($installedVSes.count -gt 1) {
        for ($i = 0; $i -lt $installedVSes.length; $i++) {
            "{0}: {1}" -f $i, $installedVSes[$i].installationName
        }
        $index = Read-Host -Prompt 'Choose VS'
        if ($index -ge 0 -and $index -lt $installedVSes.count) {
            $selected = $installedVSes[$index]
        } else {
            throw "index not in range"
        }
    } elseif ($installedVSes.count -eq 1) {
        $selected = $installedVSes[0]
    } else {
        throw "no VS found"
    }

    $instanceId = $selected.instanceId
    $installationPath = $selected.installationPath

    $devShellDll = "Microsoft.VisualStudio.DevShell.dll"
    $devShellDllPath = (Get-ChildItem -Path $installationPath -Filter $devShellDll -Recurse -ErrorAction SilentlyContinue -Force).fullname

    Import-Module $devShellDllPath
    Enter-VsDevShell -SkipAutomaticLocation -DevCmdArguments "-arch=x64" $instanceId
}

Invoke-Expression (&starship init powershell)
