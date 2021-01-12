#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#include <iostream>

const char* helloStr = "__kernel void hello(void) { }";

int main() {
    cl_int err = CL_SUCCESS;
    try {
        std::vector<cl::Platform> platforms;
        cl::Platform::get(&platforms);
        if (platforms.size() == 0) {
            std::cerr << "Platform size 0" << std::endl;
            return -1;
        }

        cl_context_properties properties[] = {
            CL_CONTEXT_PLATFORM, (cl_context_properties)(platforms[0])(), 0};
        cl::Context context(CL_DEVICE_TYPE_GPU, properties);

        std::vector<cl::Device> devices = context.getInfo<CL_CONTEXT_DEVICES>();

        cl::Program::Sources source(1, std::make_pair(helloStr, strlen(helloStr)));
        cl::Program program = cl::Program(context, source);
        program.build(devices);

        cl::Kernel kernel(program, "hello", &err);

        cl::Event event;
        cl::CommandQueue queue(context, devices[0], 0, &err);
        queue.enqueueNDRangeKernel(
            kernel, cl::NullRange, cl::NDRange(4, 4), cl::NullRange, NULL, &event
            );
        event.wait();
    } catch (cl::Error err) {
        std::cerr << "ERROR: " << err.what() << "(" << err.err() << ")" << std::endl;
    }
    return EXIT_SUCCESS;
}