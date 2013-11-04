# NSIS Manual

This library makes it easier to write NSIS Windows Installers. You should use this library if:

* You want to write a Windows Installer, and have picked NSIS (which is a common choice, and in my opinion, one of the best installer generators for Windows).

The original NSIS tool requires you to write a script in a custom programming language. This library lets you write a Haskell program using special functions that generates an NSIS script. Using this library is better because:

* **MUI2 by default:** As NSIS has evolved there are now three separate ways to define your user interface, using the Classic builtin commands, using MUI1 or using MUI2. Of these, MUI2 is by far the nicest and should always be used, but the Classic interface is far easier to use. My library always uses MUI2 and makes it easy to use.
* **Flow control:** NSIS installer scripts are written in a goto-orientated assembly language, making it hard to write any meaningful programs. In contrast, my library presents an imperative statement/expression language. Scripts end up significantly shorter and more readable.
* **Variables:** The original NSIS system has global mutable variables, 16 register variables and a stack - it directly mirrors assembly programming. In my system, variables are properly scoped and named.

If your script is simple it is likely to look relatively similar in either system. If your script is complex it could end up 100 lines shorter and far clearer in my system. As an illustrative example, let's warn before installing into the Windows directory or the System directory. In original NSIS this would be:

    StrCmp $WINDIR $INSTDIR bad 0
    StrCmp $SYSDIR $INSTDIR bad 0
    Goto skip
    bad:
    MessageBox MBOK|MB_ICON_EXCLAMATION "Warning: installing into the Windows directory"
    skip:

Using this library we can write:

    iff_ ("$INSTDIR" %== "$WINDIR" %|| "$INSTDIR" %== "$SYSDIR") $
        alert "Warning: installing into the Windows directory"

## A Simple Example

The [Examples](http://github.com/ndmitchell/nsis/Examples) directory contains a number of simple NSIS scripts, mostly ported directly from the NSIS examples. As a simple example:

    import Development.NSIS

    main = writeFile "example1.nsi" $ nsis $ do
         name "Example1"                  -- The name of the installer
         outFile "example1.exe"           -- Where to produce the installer
         installDir "$DESKTOP/Example1"   -- The default installation directory
         requestExecutionLevel User       -- Request application privileges for Windows Vista
         -- Pages to display
         page Directory                   -- Pick where to install
         page InstFiles                   -- Give a progress bar while installing
         -- Groups fo files to install
         section "" [] $ do
             setOutPath "$INSTDIR"        -- Where to install files in this section
             file [] "Example1.hs"        -- File to put into this section

The file `example1.nsi` can now be processed with `makensis` to produce the installer `example1.exe`.

## Contributions welcome

I welcome contributions. Some things you could help with:

* I currently wrap most of the standard NSIS functions, but not all of them. I welcome any additional wrappers. I have been wrapping functions by need, but eventually would like to have everything wrapped.
* The functions are mostly intended to be understood in conjunction with the NSIS documentation. I welcome any enhanced documented or work to make the documentation standalone, so people don't need to look at the underlying NSIS docs.
* I currently wrap 2 plugins - one I needed and one that made a good demo. I welcome wrappers for all plugins.

I have written this library to address my needs. I would welcome bug reports or pull requests, in particular if you can write the installer you need in NSIS but not in this library.
