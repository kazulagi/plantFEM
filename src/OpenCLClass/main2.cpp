#include <string>
#include <vector>
#include <iostream>
#include <cstdlib>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>

int main(int argc, char *argv[])
{
    try {
        std::vector<cl::Platform> platforms;
        cl::Platform::get(&platforms);
        if (platforms.size() == 0) {
            std::cerr << "No platform found." << std::endl;
            return 1;
        }
        int i = 0;
        for (auto& platform : platforms) {
            std::cout << "Platform #" << i << ":" << std::endl;
            std::cout << "  Profile: " << platform.getInfo<CL_PLATFORM_PROFILE>() << std::endl;
            std::cout << "  Name: " << platform.getInfo<CL_PLATFORM_NAME>() << std::endl;
            std::cout << "  Vendor: " << platform.getInfo<CL_PLATFORM_VENDOR>() << std::endl;
            std::cout << "  Extensions: " << platform.getInfo<CL_PLATFORM_EXTENSIONS>() << std::endl;
            std::vector<cl::Device> devices;
            platform.getDevices(CL_DEVICE_TYPE_ALL, &devices);
            int j = 0;
            for (auto& device : devices) {
                {
                    cl_device_type deviceType = device.getInfo<CL_DEVICE_TYPE>();
                    std::string deviceTypeStr = deviceType == CL_DEVICE_TYPE_CPU ? "CPU"
                        : deviceType == CL_DEVICE_TYPE_GPU ? "GPU"
                        : deviceType == CL_DEVICE_TYPE_ACCELERATOR ? "Accelerator"
                        : "unknown";
                    std::cout << "  Device #" << j << " (" << deviceTypeStr << "):" << std::endl;
                }
                std::cout << "    Name: " << device.getInfo<CL_DEVICE_NAME>() << std::endl;
                std::cout << "    Vendor: " << device.getInfo<CL_DEVICE_VENDOR>() << std::endl;
                std::cout << "    Device Version: " << device.getInfo<CL_DEVICE_VERSION>() << std::endl;
                std::cout << "    Driver Version: " << device.getInfo<CL_DRIVER_VERSION>() << std::endl;
                std::cout << "    Extensions: " << device.getInfo<CL_DEVICE_EXTENSIONS>() << std::endl;
                std::cout << "    Max Compute Units: " << device.getInfo<CL_DEVICE_MAX_COMPUTE_UNITS>() << std::endl;
                std::cout << "    Preferred Vector Width (Float): " << device.getInfo<CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT>() << std::endl;
                std::cout << "    Preferred Vector Width (Double): " << device.getInfo<CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE>() << std::endl;
                ++j;
            }
            ++i;
        }
        return 0;
    } catch (cl::Error const& ex) {
        std::cerr << "OpenCL Error: " << ex.what() << " (code " << ex.err() << ")" << std::endl;
        return 1;
    } catch (std::exception const& ex) {
        std::cerr << "Exception: " << ex.what() << std::endl;
        return 1;
    }
}
