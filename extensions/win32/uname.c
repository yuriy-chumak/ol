#include <stdio.h>
#include <windows.h>

/* long "uname" version */
struct utsname
{
	char sysname[65];  //
	char nodename[65];
	char release[65];
	char version[65];
	char machine[65];
};

__declspec(dllexport)
int uname(struct utsname* out) {
	DWORD nodenamesize = sizeof(out->nodename);
	GetComputerNameA(out->nodename, &nodenamesize);

	SYSTEM_INFO si;
	VOID (WINAPI *GetNativeSystemInfo)(LPSYSTEM_INFO) = (VOID (WINAPI*)(LPSYSTEM_INFO))
			GetProcAddress(GetModuleHandle("kernel32.dll"), "GetNativeSystemInfo");
	if (GetNativeSystemInfo)
		GetNativeSystemInfo(&si);
	else
		GetSystemInfo(&si); // todo: make as getprocaddress

	strncpy(out->sysname, "Windows", sizeof(out->sysname));
	strncpy(out->machine,
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64 ? "x86_64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM   ? "arm" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_ARM64 ? "aarch64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64  ? "ia64" :
		si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_INTEL ? "i686" :
		"Unknown", sizeof(out->machine));

	OSVERSIONINFOEXA oi;
	oi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEXA);
	if (!GetVersionExA((OSVERSIONINFOA*)&oi)) {
		DWORD dwVersion = GetVersion();
		oi.dwMajorVersion = (DWORD)(LOBYTE(LOWORD(dwVersion)));
		oi.dwMinorVersion = (DWORD)(HIBYTE(LOWORD(dwVersion)));

		if (dwVersion < 0x80000000)
			oi.dwBuildNumber = (DWORD)(HIWORD(dwVersion));
		else
			oi.dwBuildNumber = 0;
	}

	snprintf(out->release, sizeof(out->release),
		"%d.%d.%d", oi.dwMajorVersion, oi.dwMinorVersion, oi.dwBuildNumber);

	strncpy(out->version, "Windows", sizeof(out->version));
	if (oi.dwPlatformId < VER_PLATFORM_WIN32_NT) {
		oi.dwBuildNumber = (DWORD)LOWORD(oi.dwBuildNumber);
		if (oi.dwMinorVersion == 0) {
			strcat(out->version, " 95");
			if (oi.dwBuildNumber >= 1111) {
				strcat(out->version, ", OSR2");
				if (oi.dwBuildNumber >= 1212) strcat(out->version, ".5");
			}
		}
		else if (oi.dwMinorVersion == 0x90) {
			strcat(out->version, " Me");
		}
		else {
			strcat(out->version, " 98");
			if (oi.dwBuildNumber >= 2222) strcat(out->version, ", Second Edition");
		}
	}
	else {
		if (oi.dwMajorVersion <= 4) {
			strcat(out->version, " NT");
			itoa(oi.dwMajorVersion, &out->version[strlen(out->version)], 10);
			strcat(out->version, ".");
			itoa(oi.dwMinorVersion, &out->version[strlen(out->version)], 10);
			if (oi.dwMajorVersion >= 4) {
				switch (oi.wProductType) {
				case VER_NT_WORKSTATION:       strcat(out->version, ", Workstation");       break;
				case VER_NT_DOMAIN_CONTROLLER: strcat(out->version, ", Domain Controller"); break;
				case VER_NT_SERVER:            strcat(out->version, ", Server");            break;
				}
			}
		}
		else {
			switch (0x100 * oi.dwMajorVersion + oi.dwMinorVersion) {
			case 0x500:
				strcat(out->version, " 2000");
				if      (oi.wProductType == VER_NT_WORKSTATION) strcat(out->version, " Professional");
				else if (oi.wSuiteMask & VER_SUITE_DATACENTER ) strcat(out->version, " Datacenter Server");
				else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE ) strcat(out->version, " Advanced Server");
				else strcat(out->version, " Server");
				break;

			case 0x501:
				strcat(out->version, " XP");
				if (oi.wSuiteMask & VER_SUITE_PERSONAL)
					strcat(out->version, " Home Edition");
				else
					strcat(out->version, " Professional");
				break;

			case 0x502: {
				#ifndef VER_SUITE_WH_SERVER
					#define VER_SUITE_WH_SERVER 0x00008000
				#endif

				char *name, *type;

				if (GetSystemMetrics(SM_SERVERR2))
					name = "Server 2003 R2";
				else if (oi.wSuiteMask == VER_SUITE_STORAGE_SERVER)
					name = "Storage Server 2003";
				else if (oi.wSuiteMask == VER_SUITE_WH_SERVER)
					name = "Home Server";
				else if (oi.wProductType == VER_NT_WORKSTATION && si.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64) {
					name = "XP"; type = "Professional x64 Edition";
				}
				else
					name = "Server 2003";

				if (oi.wProductType != VER_NT_WORKSTATION) {
					if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_IA64) {
						if (oi.wSuiteMask & VER_SUITE_DATACENTER)
							type = "Datacenter Edition for Itanium-based Systems";
						else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE)
							type = "Enterprise Edition for Itanium-based Systems";
					}
					else if (si.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_AMD64) {
						if (oi.wSuiteMask & VER_SUITE_DATACENTER)
							type = "Datacenter x64 Edition";
						else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE)
							type = "Enterprise x64 Edition";
						else
							type = "Standard x64 Edition";
					}
					else {
						if (oi.wSuiteMask & VER_SUITE_COMPUTE_SERVER)  type = "Compute Cluster Edition";
						else if (oi.wSuiteMask & VER_SUITE_DATACENTER) type = "Datacenter Edition";
						else if (oi.wSuiteMask & VER_SUITE_ENTERPRISE) type = "Enterprise Edition";
						else if (oi.wSuiteMask & VER_SUITE_BLADE)      type = "Web Edition";
						else                                           type = "Standard Edition";
					}
				}
				snprintf(out->version, sizeof(out->version), "Windows %s, %s", name, type);
				break;
			}

			case 0x600: case 0x601: {
			//	const char* ps1;

			//	if (oi.wProductType == VER_NT_WORKSTATION)
			//		ps1 = oi.dwMinorVersion == 0 ? "Vista" : "7";
			//	else
			//		ps1 = oi.dwMinorVersion == 0 ? "Server 2008" : "Server 2008 R2";

			//   DWORD dwType = PRODUCT_UNDEFINED;
			//   if (NULL != (u.f=get_func("GetProductInfo"))) u.GetProductInfo(oi.dwMajorVersion,oi.dwMinorVersion,0,0,&dwType);
			//   switch( dwType ) {
			//	  case PRODUCT_ULTIMATE:          ps2 = "Ultimate Edition";       break;
			//	  case PRODUCT_HOME_PREMIUM:      ps2 = "Home Premium Edition";   break;
			//	  case PRODUCT_HOME_BASIC:        ps2 = "Home Basic Edition";     break;
			//	  case PRODUCT_ENTERPRISE:        ps2 = "Enterprise Edition";     break;
			//	  case PRODUCT_BUSINESS:          ps2 = "Business Edition";       break;
			//	  case PRODUCT_STARTER:           ps2 = "Starter Edition";        break;
			//	  case PRODUCT_CLUSTER_SERVER:    ps2 = "Cluster Server Edition"; break;
			//	  case PRODUCT_DATACENTER_SERVER: ps2 = "Datacenter Edition";     break;
			//	  case PRODUCT_DATACENTER_SERVER_CORE: ps2 = "Datacenter Edition (core installation)"; break;
			//	  case PRODUCT_ENTERPRISE_SERVER: ps2 = "Enterprise Edition";     break;
			//	  case PRODUCT_ENTERPRISE_SERVER_CORE: ps2 = "Enterprise Edition (core installation)"; break;
			//	  case PRODUCT_ENTERPRISE_SERVER_IA64: ps2 = "Enterprise Edition for Itanium-based Systems"; break;
			//	  case PRODUCT_SMALLBUSINESS_SERVER: ps2 = "Small Business Server"; break;
			//	  case PRODUCT_SMALLBUSINESS_SERVER_PREMIUM: ps2 = "Small Business Server Premium Edition"; break;
			//	  case PRODUCT_STANDARD_SERVER:   ps2 = "Standard Edition";       break;
			//	  case PRODUCT_STANDARD_SERVER_CORE: ps2 = "Standard Edition (core installation)"; break;
			//	  case PRODUCT_WEB_SERVER:        ps2 = "Web Server Edition";     break;
			//   }
				break;
			}
			default:
				break;
			}
		}
	}
//					   add_sp(os, oi.szCSDVersion);//*/

	return 0;
};
