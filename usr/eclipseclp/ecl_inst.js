//
// make registry entries for ECLiPSe
//
// Usage: cscript ecl_inst.js [version]
//

if (WScript.Arguments.length > 0) {
    ecl_version = WScript.Arguments(0);
} else {
    ecl_version = "6.2";
}


var WshShell = WScript.CreateObject ("WScript.Shell");

function jreHome() {
    try {
	var jre_version = WshShell.RegRead("HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\CurrentVersion");
	return WshShell.RegRead("HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\" + jre_version + "\\JavaHome");
    }
    catch(e) {
	 try {
		var jre_version = WshShell.RegRead("HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Development Kit\\CurrentVersion");
		jre_version = WshShell.RegRead("HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Development Kit\\" + jre_version + "\\JavaHome");
	 return jre_version + "\\jre"
   	 }	
         catch(e) {
		return null;
         }	
    }
}


// make ECLIPSEDIR registry entry
var script_name = WScript.ScriptFullName;
ecl_home = script_name.substring(0,script_name.lastIndexOf("\\"));

WshShell.RegWrite("HKLM\\Software\\IC-Parc\\Eclipse\\" + ecl_version
	+ "\\ECLIPSEDIR", ecl_home, "REG_SZ");


// make JRE_HOME registry entry
jre_home = jreHome();
if (jre_home != null) {
    WshShell.RegWrite("HKLM\\Software\\IC-Parc\\Eclipse\\" + ecl_version
    	+ "\\JRE_HOME", jre_home, "REG_SZ");
}
