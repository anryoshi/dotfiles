# vim: set foldmethod=marker foldlevel=0 nomodeline:
# Anton Rybakov PowerShell profile file
# Intended to be used with PS >= 7
# External dependencies:
#     vswhere  - for determine VS location
#     starship - as cross-platform prompt

$SessionEncoding = [Text.UTF8Encoding]::new()
$OutputEncoding = $SessionEncoding
[Console]::OutputEncoding = $SessionEncoding
[Console]::OutputEncoding = $SessionEncoding
$PSDefaultParameterValues['*:Encoding'] = 'utf8'

Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

function Enter-DevEnv {
    if (!(Get-Command vswhere -ErrorAction 'SilentlyContinue')) {
        throw "vswhere was not found"
    }

    $installedVSes = vswhere -prerelease -format json -products "*" | ConvertFrom-Json

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

function Write-CompilerEnvironment {
    [ordered]@{ CC = $env:CC; CC_LD = $env:CC_LD } | Format-Table -AutoSize -HideTableHeaders
}

enum Compiler {
    msvc
    clang
    clang_cl
}

function Set-CompilerEnvironment ([Compiler]$compenv = [Compiler]::msvc) {
    $envs = @{
        [Compiler]::msvc = @("cl", "link")
        [Compiler]::clang = @("clang", "lld")
        [Compiler]::clang_cl = @("cland-cl", "lld-link")
    }

    $to_set = $envs[$compenv]

    $env:CC = $to_set[0]
    $env:CC_LD = $to_set[1]

    Write-CompilerEnvironment
}

Invoke-Expression (&scoop-search --hook)

if ($env:TERM_PROGRAM -eq "WezTerm") {
    # TODO: programmatically derive from pallete
    Set-PSReadLineOption -Colors @{
        Default            = $PSStyle.Foreground.FromRgb(0x000000)
        ContinuationPrompt = $PSStyle.Foreground.FromRgb(0x595959)

        Command            = $PSStyle.Foreground.FromRgb(0x0031A9)
        Keyword            = $PSStyle.Foreground.FromRgb(0x721045)
        Parameter          = $PSStyle.Foreground.FromRgb(0x813E00)
        String             = $PSStyle.Foreground.FromRgb(0x005E00)
        Variable           = $PSStyle.Foreground.FromRgb(0x00538B)
        Type               = $PSStyle.Foreground.FromRgb(0x00538B)
        Number             = $PSStyle.Foreground.FromRgb(0x721045)
        Member             = $PSStyle.Foreground.FromRgb(0x721045)
        Operator           = $PSStyle.Foreground.FromRgb(0x000000)
        Comment            = $PSStyle.Foreground.FromRgb(0x595959)
        InlinePrediction   = $PSStyle.Foreground.FromRgb(0x595959)
        Selection          = $PSStyle.Background.FromRgb(0xBCBCBC)
        Error              = $PSStyle.Foreground.FromRgb(0xA60000)
    }
}
