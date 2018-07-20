Set-PSReadLineOption -EditMode Emacs

function Enter-DevEnv {
    if (!(Get-Command vswhere -ErrorAction 'SilentlyContinue')) {
        throw "vswhere was not found"
    }

    $instanceId = vswhere -property instanceId
    $installationPath = vswhere -property installationPath

    $devShellDll = "Microsoft.VisualStudio.DevShell.dll"
    $devShellDllPath = (Get-ChildItem -Path $installationPath -Filter $devShellDll -Recurse -ErrorAction SilentlyContinue -Force).fullname

    Import-Module $devShellDllPath
    Enter-VsDevShell -SkipAutomaticLocation -DevCmdArguments "-arch=x64" $instanceId
}

Invoke-Expression (&starship init powershell)
