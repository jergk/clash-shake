{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Clash.Shake.Gowin (
    -- * Toolchain
    Target (..),
    Board (..),
    apycula,
    gowinEDA,

    -- * Boards
    tangNano9k,
    tangConsole138
) where

import Clash.Shake
import Clash.Shake.F4PGA

import Development.Shake
import Development.Shake.FilePath

import Development.Shake.Command
import Development.Shake.Config

import qualified Text.Mustache.Compile.TH as TH
import Data.Aeson

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Maybe (mapMaybe) 
import Data.List (stripPrefix)
import Text.Mustache (renderMustache)

data Target = Target
    { targetFamily :: String
    , targetDevice :: String
    , targetVersion :: String
    }

data Board = Board
    { boardSpec :: String
    , boardTarget :: Target
    }

tangNano9k :: Board
tangNano9k = Board "tangnano9k" $ Target "GW1N-9" "GW1NR-LV9QN88PC6/I5" "C"

tangConsole138 :: Board
tangConsole138 = Board "tangconsole" $ Target "GW5AST-138" "GW5AST-LV138PG484AC1/I0" "C"

-- yosys apycula toolchain
apycula :: Board -> SynthRules
apycula Board{boardSpec, boardTarget = Target{..}} kit@ClashKit{..} outDir topName extraGenerated = do
    let pnrjson = outDir </> ("pnr" <> topName) <.> "json"
    let bitfile = outDir </> topName <.> "fs"

    json <- yosysRules kit outDir topName extraGenerated "gowin"

    pnrjson %> \out -> do
        extraFiles <- findFiles <$> extraGenerated
        let [cst] = extraFiles ["//*.cst"]

        need [cst, json]
        yosys "nextpnr-himbaechel" ["--json", json, "--write", pnrjson, "--device", targetDevice, "--vopt", "family=" <> targetFamily <> targetVersion, "--vopt", "cst=" <> cst]

    bitfile %> \out -> do
        need [pnrjson]
        cmd_ (EchoStdout True) =<< toolchain "APYCULA" "gowin_pack" ["-d", targetFamily, "-o", bitfile, pnrjson]

    pure
        SynthKit
            { bitfile = bitfile
            , phonies = ["upload" |> openFPGALoader ["-b", boardSpec] bitfile]
            }

--gowineda toolchain
targetMustache Target{..} =
    [ "pn"  .= T.pack targetDevice
    , "version"  .= T.pack targetVersion
    ]

gowinEDA :: Board -> SynthRules
gowinEDA fpga kit@ClashKit{..} outDir topName extraGenerated = do
    let projectName = topName
        rootDir = joinPath . (".." :) . map (const "..") . splitPath $ outDir

    let gowin args = cmd_  =<< toolchain "GOWIN" "gw_sh" args
        gowinIn args input = do
          gw <- toolchain "GOWIN" "gw_sh" args
          cmd_ gw (Stdin input)

    -- map extension to gowin fileType (for add_file -type ) 
    let fileTypeFromExt "v" = "verilog"
        fileTypeFromExt ext = ext
        fileTypeFromPath = fileTypeFromExt . drop 1 . takeExtension

    -- find ModuleName for ipc 
    let getIpcWithMod ipc = do
          ipcMod <- mapMaybe (stripPrefix "module=")  <$> readFileLines ipc
          case ipcMod of
              [] -> fail ("Module Name not found for ipc: " <> ipc)
              [modName] -> pure (ipc,modName)
              modNames -> fail ("Found multiple Modules names (" <> show modNames <> ") for ipc" <> ipc)

    -- copy ipc from extra files to outDir so ipc artifacts get generated in out
    let buildSrc = outDir </> projectName </>"src"
    let ipcCopys = buildSrc <//> "*" <.> "ipc"
    ipcCopys %> \out -> do
        let src = makeRelative buildSrc out
        extraFiles <- findFiles <$> extraGenerated
        case extraFiles ["//" <> src] of
          [oneFile] -> copyFileChanged oneFile out
          [] -> error $ unwords ["Cannot find IPC file", src]
          multiple ->  error $ unwords ["Multiple candidates for IPC file", src, show multiple]
    
    -- generate project tcl
    let projectTcl = outDir </> "project" <.> "tcl"
    projectTcl %> \out -> do
        putInfo $ "generating the project tcl: " <> projectTcl

        clashSrcs <- manifestSrcs

        -- dirty fix for nettype bug in clash (gowin ips rely on default nettype)
        -- has been fixed in clash master (or >1.9)

#if defined(MIN_VERSION_clash_ghc)
#if MIN_VERSION_clash_ghc(1,9,0)
#else
        sequence_ [command_ [Shell] ("echo '`default_nettype wire' >>" <>src) [] | src <- clashSrcs]
        putInfo $ "Dirty fix for clash <1.9.0 applied: patching clash output to reset to default nettype. Clash may complain about modified output which requires a clean rebuild to fix"
#endif
#endif

        extraFiles <- findFiles <$> extraGenerated

        let extras = extraFiles $ map ("//*"<>) ["v","vhdl","sv","sdc", "cst", "gsc","fi","mi","rao","gvio", "gpa"]

        -- disabled since untested. When to source?  
        putInfo $ "ignoring tcl files: " <> show (extraFiles ["//*.tcl"])
        -- let tcls = extraFiles ["//*.tcl"]
        let tcls = [] :: [FilePath]
        
        --ipcs seem to currently crash gw_sh so they are ignored 
        putInfo $ "ignoring ipc files: " <> show (extraFiles ["//*.ipc"])
        --let ipcs = map (buildSrc </>) $ extraFiles ["//*.ipc"]
        let ipcs = [] :: [FilePath]

        ips <- mapM getIpcWithMod ipcs

        need ipcs
        need clashSrcs
        need extras
        need tcls

        putInfo $ "found these src files: " <> show  clashSrcs
        putInfo $ "found these additional files: " <> show extras
        putInfo $ "found these ipcore files: " <> show ipcs

        let template = $(TH.compileMustacheFile "template/gowin-ide/project.tcl.mustache")
        let values = object . mconcat $
                [ [ "project" .= T.pack projectName ]
                , [ "top" .= T.pack topName ]
                , targetMustache $ boardTarget fpga
                , [ "outDir" .= outDir]
                , [ "tcls" .= [ object [ "filePath" .= (rootDir </> path)] | path <- tcls]]
                , [ "ipcs" .= [object ["filePath" .= (rootDir </> path), "name" .= name] | (path,name) <- ips]]
                , [ "srcs" .= [ object [ "filePath" .= (rootDir </> path), "fileType" .= fileTypeFromPath path] | path <- extras <> clashSrcs]]
                ]
        writeFileChanged out . TL.unpack $ renderMustache template values
        putInfo $ "created" <> projectTcl

    let projectFile =  outDir </> projectName </> projectName <.> "gprj"
    projectFile %> \_out -> do
        need [outDir </> "project.tcl"]
        putInfo $ "creating project: " <> projectFile
        gowin [outDir </> "project.tcl"]

    let netList = outDir </> projectName </> "impl" </> "gwsynthesis" </> topName <.> "vg"
    netList %> \_out -> do
        need [projectFile]
        putInfo "running synth"

        let tclCmdRunSynth = "open_project " <> projectFile <> "; run syn; run close"
        gowinIn [] tclCmdRunSynth

    let bitfile = outDir </> projectName </> "impl" </> "pnr" </> topName <.> "fs"
    bitfile %> \_out -> do
        need [netList]
        putInfo "running pnr"

        let tclCmdRunPNR = "open_project " <> projectFile <> "; run pnr; run close"
        gowinIn [] tclCmdRunPNR


    pure $ SynthKit
        { bitfile = bitfile
          , phonies =
              [
               "project" |> do
                    need [projectFile]
              , "synth" |> do
                    need [netList]
              ,"upload" |> do
                    need [bitfile]
                    openFPGALoader ["-b", boardSpec fpga] bitfile
              ]
        }
