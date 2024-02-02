# Derived from https://github.com/microsoft/vcpkg/blob/20584b9d4405af7052769381e5ba85367c35ec92/triplets/community/x64-mingw-dynamic.cmake

set(VCPKG_TARGET_ARCHITECTURE x64)
set(VCPKG_CRT_LINKAGE dynamic)
set(VCPKG_LIBRARY_LINKAGE dynamic)
set(VCPKG_ENV_PASSTHROUGH PATH)

set(VCPKG_CMAKE_SYSTEM_NAME MinGW)
set(VCPKG_POLICY_DLLS_WITHOUT_LIBS enabled)

# nimskull customization
set(VCPKG_BUILD_TYPE release)
set(VCPKG_LINKER_FLAGS_RELEASE "-Wl,--no-insert-timestamp")

