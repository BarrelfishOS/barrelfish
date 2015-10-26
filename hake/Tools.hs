--------------------------------------------------------------------------
-- Copyright (c) 2015 ETH Zurich.
-- All rights reserved.
--
-- This file is distributed under the terms in the attached LICENSE file.
-- If you do not find this file, copies can be found by writing to:
-- ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
-- Attn: Systems Group.
--
-- Toolchain definitions for Hake
-- 
--------------------------------------------------------------------------

module Tools where

import System.FilePath

findTool path prefix tool = path </> (prefix ++ tool)

data ToolDetails =
    ToolDetails {
        toolPath :: FilePath,
        toolPrefix :: String
    }

--
-- ARM Cortex-A little-endian toolchains (armv7,armv5)
--

-- System (Ubuntu) ARM toolchain
arm_system
    = ToolDetails {
        toolPath = "",
        toolPrefix = "arm-linux-gnueabi-"
      }

-- Linaro 2015.06 (GCC 4.8)
arm_netos_linaro_2015_06
    = ToolDetails {
        toolPath = "/home/netos/tools/linaro" </>
                   "gcc-linaro-4.8-2015.06-x86_64_arm-eabi" </>
                   "bin",
        toolPrefix = "arm-eabi-"
      }

-- Linaro 2015.05 (GCC 4.9)
arm_netos_linaro_2015_05
    = ToolDetails {
        toolPath = "/home/netos/tools/linaro" </>
                   "gcc-linaro-4.9-2015.05-x86_64_arm-eabi" </>
                   "bin",
        toolPrefix = "arm-eabi-"
      }

-- Linaro 2015.02 (GCC 4.9)
arm_netos_linaro_2015_02
    = ToolDetails {
        toolPath = "/home/netos/tools/linaro" </>
                   "gcc-linaro-4.9-2015.02-3-x86_64_arm-eabi" </>
                   "bin",
        toolPrefix = "arm-eabi-"
      }

-- Linaro 2014.11 (GCC 4.9)
arm_netos_linaro_2014_11
    = ToolDetails {
        toolPath = "/home/netos/tools/linaro" </>
                   "gcc-linaro-4.9-2014.11-x86_64_arm-eabi" </>
                   "bin",
        toolPrefix = "arm-eabi-"
      }

--
-- ARM Cortex-M little-endian toolchains (armv7m)
--

-- ARM-GCC 2014q4 (GCC 4.9)
arm_netos_arm_2014q4
    = ToolDetails {
        toolPath = "/home/netos/tools/gcc-arm-embedded" </>
                   "gcc-arm-none-eabi-4_9-2014q4" </>
                   "bin",
        toolPrefix = "arm-none-eabi-"
      }

-- ARM-GCC 2015q1 (GCC 4.9)
arm_netos_arm_2015q1
    = ToolDetails {
        toolPath = "/home/netos/tools/gcc-arm-embedded" </>
                   "gcc-arm-none-eabi-4_9-2015q1" </>
                   "bin",
        toolPrefix = "arm-none-eabi-"
      }

-- ARM-GCC 2015q2 (GCC 4.9)
arm_netos_arm_2015q2
    = ToolDetails {
        toolPath = "/home/netos/tools/gcc-arm-embedded" </>
                   "gcc-arm-none-eabi-4_9-2015q2" </>
                   "bin",
        toolPrefix = "arm-none-eabi-"
      }

--
-- ARM big-endian toolchains (xscale)
--

-- Linaro 2015.02 (GCC 4.9)
arm_netos_linaro_be_2015_02
    = ToolDetails {
        toolPath = "/home/netos/tools/linaro" </>
                   "gcc-linaro-4.9-2015.02-3-x86_64_armeb-eabi" </>
                   "bin",
        toolPrefix = "armeb-eabi-"
      }

--
-- X86 (32/64) toolchains (x86_32,x86_64)
--

-- System (Ubuntu) ARM toolchain
x86_system
    = ToolDetails {
        toolPath = "",
        toolPrefix = "x86_64-linux-gnu-"
      }

--
-- Xeon Phi toolchains (k1om)
--

-- Intel MPSS 3.4 (GCC 4.7)
k1om_netos_mpss_3_4
    = ToolDetails {
        toolPath = "/home/netos/tools" </>
                   "mpss-3.4/x86_64-mpsssdk-linux" </> 
                   "usr/bin/k1om-mpss-linux",
        toolPrefix = "k1om-mpss-linux-"
      }
