; OpenCL 1.0 (2009)

(define-library (OpenCL version-1-0)
   (export
    CL_VERSION_1_0

      CL_LIBRARY  ;internal variable

      ; CL base types
      cl_char     ;   signed  8-bit
      cl_uchar    ; unsigned  8-bit
      cl_short    ;   signed 16-bit
      cl_ushort   ; unsigned 16-bit
      cl_int      ;   signed 32-bit
      cl_uint     ; unsigned 32-bit
      cl_long     ;   signed 64-bit
      cl_ulong    ; unsigned 64-bit

      cl_half     ; unsigned 16-bit
      cl_float    ; floating 32-bit
      cl_double   ; floating 64-bit

      ; CL platform types
;      cl_platform_id*
;      cl_device_id*
;      cl_context*
;      cl_command_queue*
;      cl_mem*
;      cl_program*
;      cl_kernel*
;      cl_event*
;      cl_sampler*

;      GLvoid   GLvoid*

      CL_SUCCESS
      CL_DEVICE_NOT_FOUND
      CL_DEVICE_NOT_AVAILABLE
      CL_COMPILER_NOT_AVAILABLE
      CL_MEM_OBJECT_ALLOCATION_FAILURE
      CL_OUT_OF_RESOURCES
      CL_OUT_OF_HOST_MEMORY
      CL_PROFILING_INFO_NOT_AVAILABLE
      CL_MEM_COPY_OVERLAP
      CL_IMAGE_FORMAT_MISMATCH
      CL_IMAGE_FORMAT_NOT_SUPPORTED
      CL_BUILD_PROGRAM_FAILURE
      CL_MAP_FAILURE

      CL_INVALID_VALUE
      CL_INVALID_DEVICE_TYPE
      CL_INVALID_PLATFORM
      CL_INVALID_DEVICE
      CL_INVALID_CONTEXT
      CL_INVALID_QUEUE_PROPERTIES
      CL_INVALID_COMMAND_QUEUE
      CL_INVALID_HOST_PTR
      CL_INVALID_MEM_OBJECT
      CL_INVALID_IMAGE_FORMAT_DESCRIPTOR
      CL_INVALID_IMAGE_SIZE
      CL_INVALID_SAMPLER
      CL_INVALID_BINARY
      CL_INVALID_BUILD_OPTIONS
      CL_INVALID_PROGRAM
      CL_INVALID_PROGRAM_EXECUTABLE
      CL_INVALID_KERNEL_NAME
      CL_INVALID_KERNEL_DEFINITION
      CL_INVALID_KERNEL
      CL_INVALID_ARG_INDEX
      CL_INVALID_ARG_VALUE
      CL_INVALID_ARG_SIZE
      CL_INVALID_KERNEL_ARGS
      CL_INVALID_WORK_DIMENSION
      CL_INVALID_WORK_GROUP_SIZE
      CL_INVALID_WORK_ITEM_SIZE
      CL_INVALID_GLOBAL_OFFSET
      CL_INVALID_EVENT_WAIT_LIST
      CL_INVALID_EVENT
      CL_INVALID_OPERATION
      CL_INVALID_GL_OBJECT
      CL_INVALID_BUFFER_SIZE
      CL_INVALID_MIP_LEVEL
      CL_INVALID_GLOBAL_WORK_SIZE


      CL_TRUE CL_FALSE


      clGetPlatformIDs

      clGetPlatformInfo
         CL_PLATFORM_PROFILE
         CL_PLATFORM_VERSION
         CL_PLATFORM_NAME
         CL_PLATFORM_VENDOR
         CL_PLATFORM_EXTENSIONS

      clGetDeviceIDs
         CL_DEVICE_TYPE_DEFAULT
         CL_DEVICE_TYPE_CPU
         CL_DEVICE_TYPE_GPU
         CL_DEVICE_TYPE_ACCELERATOR
         CL_DEVICE_TYPE_ALL

      clGetDeviceInfo
         CL_DEVICE_TYPE
         CL_DEVICE_VENDOR_ID
         CL_DEVICE_MAX_COMPUTE_UNITS
         CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
         CL_DEVICE_MAX_WORK_GROUP_SIZE
         CL_DEVICE_MAX_WORK_ITEM_SIZES
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT
         CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE
         CL_DEVICE_MAX_CLOCK_FREQUENCY
         CL_DEVICE_ADDRESS_BITS
         CL_DEVICE_MAX_READ_IMAGE_ARGS
         CL_DEVICE_MAX_WRITE_IMAGE_ARGS
         CL_DEVICE_MAX_MEM_ALLOC_SIZE
         CL_DEVICE_IMAGE2D_MAX_WIDTH
         CL_DEVICE_IMAGE2D_MAX_HEIGHT
         CL_DEVICE_IMAGE3D_MAX_WIDTH
         CL_DEVICE_IMAGE3D_MAX_HEIGHT
         CL_DEVICE_IMAGE3D_MAX_DEPTH
         CL_DEVICE_IMAGE_SUPPORT
         CL_DEVICE_MAX_PARAMETER_SIZE
         CL_DEVICE_MAX_SAMPLERS
         CL_DEVICE_MEM_BASE_ADDR_ALIGN
         CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE
         CL_DEVICE_SINGLE_FP_CONFIG
         CL_DEVICE_GLOBAL_MEM_CACHE_TYPE
         CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE
         CL_DEVICE_GLOBAL_MEM_CACHE_SIZE
         CL_DEVICE_GLOBAL_MEM_SIZE
         CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
         CL_DEVICE_MAX_CONSTANT_ARGS
         CL_DEVICE_LOCAL_MEM_TYPE
         CL_DEVICE_LOCAL_MEM_SIZE
         CL_DEVICE_ERROR_CORRECTION_SUPPORT
         CL_DEVICE_PROFILING_TIMER_RESOLUTION
         CL_DEVICE_ENDIAN_LITTLE
         CL_DEVICE_AVAILABLE
         CL_DEVICE_COMPILER_AVAILABLE
         CL_DEVICE_EXECUTION_CAPABILITIES
         CL_DEVICE_QUEUE_PROPERTIES
         CL_DEVICE_NAME
         CL_DEVICE_VENDOR
         CL_DRIVER_VERSION
         CL_DEVICE_PROFILE
         CL_DEVICE_VERSION
         CL_DEVICE_EXTENSIONS
         CL_DEVICE_PLATFORM

      clCreateContext


      ; ...

      clCreateProgramWithSource
      ;clCreateProgramWithBinary
      ;clRetainProgram
      ;clReleaseProgram
      clBuildProgram
      ;clUnloadCompiler
      ;clGetProgramInfo
      clGetProgramBuildInfo
         CL_PROGRAM_BUILD_STATUS
         CL_PROGRAM_BUILD_OPTIONS
         CL_PROGRAM_BUILD_LOG


   )
; ============================================================================
; == implementation ==========================================================
   (import
      (r5rs core) (owl math) (owl io) (owl string)
      (otus ffi)
      (owl interop) (owl list))

(begin
   (define CL_VERSION_1_0 1)

   (define type-int64 44)

   (define cl_char   type-fix+)
   (define cl_uchar  type-fix+)
   (define cl_short  type-fix+)
   (define cl_ushort type-fix+)
   (define cl_int    type-int+)  (define cl_int*  type-vector-raw)
   (define cl_uint   type-int+)  (define cl_uint* type-vector-raw)
   (define cl_long   type-int64)
   (define cl_ulong  type-int64)
   (define cl_half   type-fix+) ;?
   (define cl_float  fft-float)
   (define cl_double type-double)

   (define cl_platform_id type-int+)
   (define cl_device_id type-int+)

   (define cl_platform_id* type-vector-raw)
   (define cl_device_id* type-vector-raw)
   (define cl_context type-port)
;   (define cl_command_queue* type-port)
;   (define cl_mem* type-port)
   (define cl_program type-port)
;   (define cl_kernel* type-port)
;   (define cl_event* type-port)
;   (define cl_sampler* type-port)


   (define cl_bool cl_uint)
   (define cl_bitfield cl_ulong)
   (define cl_device_type cl_bitfield)
   (define cl_platform_info cl_uint)
   (define cl_device_info cl_uint)
   (define cl_device_address_info cl_bitfield)
   (define cl_device_fp_config cl_bitfield)
   (define cl_device_mem_cache_type cl_uint)
   (define cl_device_local_mem_type cl_uint)
   (define cl_device_exec_capabilities cl_bitfield)
   (define cl_command_queue_properties cl_bitfield)

   (define cl_context_properties* type-vector-raw)
   (define cl_context_info cl_uint)
   (define cl_command_queue_info cl_uint)
   (define cl_channel_order cl_uint)
   (define cl_channel_type cl_uint)
   (define cl_mem_flags cl_bitfield)
   (define cl_mem_object_type cl_uint)
   (define cl_mem_info cl_uint)
   (define cl_image_info cl_uint)
   (define cl_addressing_mode cl_uint)
   (define cl_filter_mode cl_uint)
   (define cl_sampler_info cl_uint)
   (define cl_map_flags cl_bitfield)
   (define cl_program_info cl_uint)
   (define cl_program_build_info cl_uint)
   (define cl_build_status cl_int)
   (define cl_kernel_info cl_uint)
   (define cl_kernel_work_group_info cl_uint)
   (define cl_event_info cl_uint)
   (define cl_command_type cl_uint)
   (define cl_profiling_info cl_uint)


;/* Error Codes */
(define CL_SUCCESS                                  0)
(define CL_DEVICE_NOT_FOUND                         (- (<< 1 32) 1))
(define CL_DEVICE_NOT_AVAILABLE                     (- 16777216  2))
(define CL_COMPILER_NOT_AVAILABLE                   (- 16777216  3))
(define CL_MEM_OBJECT_ALLOCATION_FAILURE            (- 16777216  4))
(define CL_OUT_OF_RESOURCES                         (- 16777216  5))
(define CL_OUT_OF_HOST_MEMORY                       (- 16777216  6))
(define CL_PROFILING_INFO_NOT_AVAILABLE             (- 16777216  7))
(define CL_MEM_COPY_OVERLAP                         (- 16777216  8))
(define CL_IMAGE_FORMAT_MISMATCH                    (- 16777216  9))
(define CL_IMAGE_FORMAT_NOT_SUPPORTED               (- 16777216 10))
(define CL_BUILD_PROGRAM_FAILURE                    (- 16777216 11))
(define CL_MAP_FAILURE                              (- 16777216 12))

(define CL_INVALID_VALUE                            (- 16777216 30))
(define CL_INVALID_DEVICE_TYPE                      (- 16777216 31))
(define CL_INVALID_PLATFORM                         (- 16777216 32))
(define CL_INVALID_DEVICE                           (- 16777216 33))
(define CL_INVALID_CONTEXT                          (- 16777216 34))
(define CL_INVALID_QUEUE_PROPERTIES                 (- 16777216 35))
(define CL_INVALID_COMMAND_QUEUE                    (- 16777216 36))
(define CL_INVALID_HOST_PTR                         (- 16777216 37))
(define CL_INVALID_MEM_OBJECT                       (- 16777216 38))
(define CL_INVALID_IMAGE_FORMAT_DESCRIPTOR          (- 16777216 39))
(define CL_INVALID_IMAGE_SIZE                       (- 16777216 40))
(define CL_INVALID_SAMPLER                          (- 16777216 41))
(define CL_INVALID_BINARY                           (- 16777216 42))
(define CL_INVALID_BUILD_OPTIONS                    (- 16777216 43))
(define CL_INVALID_PROGRAM                          (- 16777216 44))
(define CL_INVALID_PROGRAM_EXECUTABLE               (- 16777216 45))
(define CL_INVALID_KERNEL_NAME                      (- 16777216 46))
(define CL_INVALID_KERNEL_DEFINITION                (- 16777216 47))
(define CL_INVALID_KERNEL                           (- 16777216 48))
(define CL_INVALID_ARG_INDEX                        (- 16777216 49))
(define CL_INVALID_ARG_VALUE                        (- 16777216 50))
(define CL_INVALID_ARG_SIZE                         (- 16777216 51))
(define CL_INVALID_KERNEL_ARGS                      (- 16777216 52))
(define CL_INVALID_WORK_DIMENSION                   (- 16777216 53))
(define CL_INVALID_WORK_GROUP_SIZE                  (- 16777216 54))
(define CL_INVALID_WORK_ITEM_SIZE                   (- 16777216 55))
(define CL_INVALID_GLOBAL_OFFSET                    (- 16777216 56))
(define CL_INVALID_EVENT_WAIT_LIST                  (- 16777216 57))
(define CL_INVALID_EVENT                            (- 16777216 58))
(define CL_INVALID_OPERATION                        (- 16777216 59))
(define CL_INVALID_GL_OBJECT                        (- 16777216 60))
(define CL_INVALID_BUFFER_SIZE                      (- 16777216 61))
(define CL_INVALID_MIP_LEVEL                        (- 16777216 62))
(define CL_INVALID_GLOBAL_WORK_SIZE                 (- 16777216 63))

;/* cl_bool */
(define CL_FALSE                                    0)
(define CL_TRUE                                     1)

;/* cl_platform_info */
(define CL_PLATFORM_PROFILE                         #x0900)
(define CL_PLATFORM_VERSION                         #x0901)
(define CL_PLATFORM_NAME                            #x0902)
(define CL_PLATFORM_VENDOR                          #x0903)
(define CL_PLATFORM_EXTENSIONS                      #x0904)

;/* cl_device_type - bitfield */
(define CL_DEVICE_TYPE_DEFAULT                      (<< 1 0))
(define CL_DEVICE_TYPE_CPU                          (<< 1 1))
(define CL_DEVICE_TYPE_GPU                          (<< 1 2))
(define CL_DEVICE_TYPE_ACCELERATOR                  (<< 1 3))
(define CL_DEVICE_TYPE_ALL                          #xFFFFFFFF)

;/* cl_device_info */
(define CL_DEVICE_TYPE                              #x1000)
(define CL_DEVICE_VENDOR_ID                         #x1001)
(define CL_DEVICE_MAX_COMPUTE_UNITS                 #x1002)
(define CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS          #x1003)
(define CL_DEVICE_MAX_WORK_GROUP_SIZE               #x1004)
(define CL_DEVICE_MAX_WORK_ITEM_SIZES               #x1005)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR       #x1006)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT      #x1007)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT        #x1008)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG       #x1009)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT      #x100A)
(define CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE     #x100B)
(define CL_DEVICE_MAX_CLOCK_FREQUENCY               #x100C)
(define CL_DEVICE_ADDRESS_BITS                      #x100D)
(define CL_DEVICE_MAX_READ_IMAGE_ARGS               #x100E)
(define CL_DEVICE_MAX_WRITE_IMAGE_ARGS              #x100F)
(define CL_DEVICE_MAX_MEM_ALLOC_SIZE                #x1010)
(define CL_DEVICE_IMAGE2D_MAX_WIDTH                 #x1011)
(define CL_DEVICE_IMAGE2D_MAX_HEIGHT                #x1012)
(define CL_DEVICE_IMAGE3D_MAX_WIDTH                 #x1013)
(define CL_DEVICE_IMAGE3D_MAX_HEIGHT                #x1014)
(define CL_DEVICE_IMAGE3D_MAX_DEPTH                 #x1015)
(define CL_DEVICE_IMAGE_SUPPORT                     #x1016)
(define CL_DEVICE_MAX_PARAMETER_SIZE                #x1017)
(define CL_DEVICE_MAX_SAMPLERS                      #x1018)
(define CL_DEVICE_MEM_BASE_ADDR_ALIGN               #x1019)
(define CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE          #x101A)
(define CL_DEVICE_SINGLE_FP_CONFIG                  #x101B)
(define CL_DEVICE_GLOBAL_MEM_CACHE_TYPE             #x101C)
(define CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE         #x101D)
(define CL_DEVICE_GLOBAL_MEM_CACHE_SIZE             #x101E)
(define CL_DEVICE_GLOBAL_MEM_SIZE                   #x101F)
(define CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE          #x1020)
(define CL_DEVICE_MAX_CONSTANT_ARGS                 #x1021)
(define CL_DEVICE_LOCAL_MEM_TYPE                    #x1022)
(define CL_DEVICE_LOCAL_MEM_SIZE                    #x1023)
(define CL_DEVICE_ERROR_CORRECTION_SUPPORT          #x1024)
(define CL_DEVICE_PROFILING_TIMER_RESOLUTION        #x1025)
(define CL_DEVICE_ENDIAN_LITTLE                     #x1026)
(define CL_DEVICE_AVAILABLE                         #x1027)
(define CL_DEVICE_COMPILER_AVAILABLE                #x1028)
(define CL_DEVICE_EXECUTION_CAPABILITIES            #x1029)
(define CL_DEVICE_QUEUE_PROPERTIES                  #x102A)
(define CL_DEVICE_NAME                              #x102B)
(define CL_DEVICE_VENDOR                            #x102C)
(define CL_DRIVER_VERSION                           #x102D)
(define CL_DEVICE_PROFILE                           #x102E)
(define CL_DEVICE_VERSION                           #x102F)
(define CL_DEVICE_EXTENSIONS                        #x1030)
(define CL_DEVICE_PLATFORM                          #x1031)
;/* 0x1032 reserved for CL_DEVICE_DOUBLE_FP_CONFIG */
;/* 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG */

;/* cl_device_fp_config - bitfield */
;#define CL_FP_DENORM                                (1 << 0)
;#define CL_FP_INF_NAN                               (1 << 1)
;#define CL_FP_ROUND_TO_NEAREST                      (1 << 2)
;#define CL_FP_ROUND_TO_ZERO                         (1 << 3)
;#define CL_FP_ROUND_TO_INF                          (1 << 4)
;#define CL_FP_FMA                                   (1 << 5)

;/* cl_device_mem_cache_type */
;#define CL_NONE                                     0x0
;#define CL_READ_ONLY_CACHE                          0x1
;#define CL_READ_WRITE_CACHE                         0x2

;/* cl_device_local_mem_type */
;#define CL_LOCAL                                    0x1
;#define CL_GLOBAL                                   0x2

;/* cl_device_exec_capabilities - bitfield */
;#define CL_EXEC_KERNEL                              (1 << 0)
;#define CL_EXEC_NATIVE_KERNEL                       (1 << 1)

;/* cl_command_queue_properties - bitfield */
;#define CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE      (1 << 0)
;#define CL_QUEUE_PROFILING_ENABLE                   (1 << 1)

;/* cl_context_info  */
;#define CL_CONTEXT_REFERENCE_COUNT                  0x1080
;#define CL_CONTEXT_DEVICES                          0x1081
;#define CL_CONTEXT_PROPERTIES                       0x1082

;/* cl_context_info + cl_context_properties */
;#define CL_CONTEXT_PLATFORM                         0x1084

;/* cl_command_queue_info */
;#define CL_QUEUE_CONTEXT                            0x1090
;#define CL_QUEUE_DEVICE                             0x1091
;#define CL_QUEUE_REFERENCE_COUNT                    0x1092
;#define CL_QUEUE_PROPERTIES                         0x1093

;/* cl_mem_flags - bitfield */
;#define CL_MEM_READ_WRITE                           (1 << 0)
;#define CL_MEM_WRITE_ONLY                           (1 << 1)
;#define CL_MEM_READ_ONLY                            (1 << 2)
;#define CL_MEM_USE_HOST_PTR                         (1 << 3)
;#define CL_MEM_ALLOC_HOST_PTR                       (1 << 4)
;#define CL_MEM_COPY_HOST_PTR                        (1 << 5)

;/* cl_channel_order */
;#define CL_R                                        0x10B0
;#define CL_A                                        0x10B1
;#define CL_RG                                       0x10B2
;#define CL_RA                                       0x10B3
;#define CL_RGB                                      0x10B4
;#define CL_RGBA                                     0x10B5
;#define CL_BGRA                                     0x10B6
;#define CL_ARGB                                     0x10B7
;#define CL_INTENSITY                                0x10B8
;#define CL_LUMINANCE                                0x10B9

;/* cl_channel_type */
;#define CL_SNORM_INT8                               0x10D0
;#define CL_SNORM_INT16                              0x10D1
;#define CL_UNORM_INT8                               0x10D2
;#define CL_UNORM_INT16                              0x10D3
;#define CL_UNORM_SHORT_565                          0x10D4
;#define CL_UNORM_SHORT_555                          0x10D5
;#define CL_UNORM_INT_101010                         0x10D6
;#define CL_SIGNED_INT8                              0x10D7
;#define CL_SIGNED_INT16                             0x10D8
;#define CL_SIGNED_INT32                             0x10D9
;#define CL_UNSIGNED_INT8                            0x10DA
;#define CL_UNSIGNED_INT16                           0x10DB
;#define CL_UNSIGNED_INT32                           0x10DC
;#define CL_HALF_FLOAT                               0x10DD
;#define CL_FLOAT                                    0x10DE

;/* cl_mem_object_type */
;#define CL_MEM_OBJECT_BUFFER                        0x10F0
;#define CL_MEM_OBJECT_IMAGE2D                       0x10F1
;#define CL_MEM_OBJECT_IMAGE3D                       0x10F2

;/* cl_mem_info */
;#define CL_MEM_TYPE                                 0x1100
;#define CL_MEM_FLAGS                                0x1101
;#define CL_MEM_SIZE                                 0x1102
;#define CL_MEM_HOST_PTR                             0x1103
;#define CL_MEM_MAP_COUNT                            0x1104
;#define CL_MEM_REFERENCE_COUNT                      0x1105
;#define CL_MEM_CONTEXT                              0x1106

;/* cl_image_info */
;#define CL_IMAGE_FORMAT                             0x1110
;#define CL_IMAGE_ELEMENT_SIZE                       0x1111
;#define CL_IMAGE_ROW_PITCH                          0x1112
;#define CL_IMAGE_SLICE_PITCH                        0x1113
;#define CL_IMAGE_WIDTH                              0x1114
;#define CL_IMAGE_HEIGHT                             0x1115
;#define CL_IMAGE_DEPTH                              0x1116

;/* cl_addressing_mode */
;#define CL_ADDRESS_NONE                             0x1130
;#define CL_ADDRESS_CLAMP_TO_EDGE                    0x1131
;#define CL_ADDRESS_CLAMP                            0x1132
;#define CL_ADDRESS_REPEAT                           0x1133

;/* cl_filter_mode */
;#define CL_FILTER_NEAREST                           0x1140
;#define CL_FILTER_LINEAR                            0x1141

;/* cl_sampler_info */
;#define CL_SAMPLER_REFERENCE_COUNT                  0x1150
;#define CL_SAMPLER_CONTEXT                          0x1151
;#define CL_SAMPLER_NORMALIZED_COORDS                0x1152
;#define CL_SAMPLER_ADDRESSING_MODE                  0x1153
;#define CL_SAMPLER_FILTER_MODE                      0x1154

;/* cl_map_flags - bitfield */
;#define CL_MAP_READ                                 (1 << 0)
;#define CL_MAP_WRITE                                (1 << 1)

;/* cl_program_info */
;#define CL_PROGRAM_REFERENCE_COUNT                  0x1160
;#define CL_PROGRAM_CONTEXT                          0x1161
;#define CL_PROGRAM_NUM_DEVICES                      0x1162
;#define CL_PROGRAM_DEVICES                          0x1163
;#define CL_PROGRAM_SOURCE                           0x1164
;#define CL_PROGRAM_BINARY_SIZES                     0x1165
;#define CL_PROGRAM_BINARIES                         0x1166

;/* cl_program_build_info */
(define CL_PROGRAM_BUILD_STATUS                     #x1181)
(define CL_PROGRAM_BUILD_OPTIONS                    #x1182)
(define CL_PROGRAM_BUILD_LOG                        #x1183)

;/* cl_build_status */
;#define CL_BUILD_SUCCESS                            0
;#define CL_BUILD_NONE                               -1
;#define CL_BUILD_ERROR                              -2
;#define CL_BUILD_IN_PROGRESS                        -3

;/* cl_kernel_info */
;#define CL_KERNEL_FUNCTION_NAME                     0x1190
;#define CL_KERNEL_NUM_ARGS                          0x1191
;#define CL_KERNEL_REFERENCE_COUNT                   0x1192
;#define CL_KERNEL_CONTEXT                           0x1193
;#define CL_KERNEL_PROGRAM                           0x1194

;/* cl_kernel_work_group_info */
;#define CL_KERNEL_WORK_GROUP_SIZE                   0x11B0
;#define CL_KERNEL_COMPILE_WORK_GROUP_SIZE           0x11B1
;#define CL_KERNEL_LOCAL_MEM_SIZE                    0x11B2

;/* cl_event_info  */
;#define CL_EVENT_COMMAND_QUEUE                      0x11D0
;#define CL_EVENT_COMMAND_TYPE                       0x11D1
;#define CL_EVENT_REFERENCE_COUNT                    0x11D2
;#define CL_EVENT_COMMAND_EXECUTION_STATUS           0x11D3

;/* cl_command_type */
;#define CL_COMMAND_NDRANGE_KERNEL                   0x11F0
;#define CL_COMMAND_TASK                             0x11F1
;#define CL_COMMAND_NATIVE_KERNEL                    0x11F2
;#define CL_COMMAND_READ_BUFFER                      0x11F3
;#define CL_COMMAND_WRITE_BUFFER                     0x11F4
;#define CL_COMMAND_COPY_BUFFER                      0x11F5
;#define CL_COMMAND_READ_IMAGE                       0x11F6
;#define CL_COMMAND_WRITE_IMAGE                      0x11F7
;#define CL_COMMAND_COPY_IMAGE                       0x11F8
;#define CL_COMMAND_COPY_IMAGE_TO_BUFFER             0x11F9
;#define CL_COMMAND_COPY_BUFFER_TO_IMAGE             0x11FA
;#define CL_COMMAND_MAP_BUFFER                       0x11FB
;#define CL_COMMAND_MAP_IMAGE                        0x11FC
;#define CL_COMMAND_UNMAP_MEM_OBJECT                 0x11FD
;#define CL_COMMAND_MARKER                           0x11FE
;#define CL_COMMAND_ACQUIRE_GL_OBJECTS               0x11FF
;#define CL_COMMAND_RELEASE_GL_OBJECTS               0x1200

;/* command execution status */
;#define CL_COMPLETE                                 0x0
;#define CL_RUNNING                                  0x1
;#define CL_SUBMITTED                                0x2
;#define CL_QUEUED                                   0x3

;/* cl_profiling_info  */
;#define CL_PROFILING_COMMAND_QUEUED                 0x1280
;#define CL_PROFILING_COMMAND_SUBMIT                 0x1281
;#define CL_PROFILING_COMMAND_START                  0x1282
;#define CL_PROFILING_COMMAND_END                    0x1283


; https://en.wikipedia.org/wiki/Uname
(define uname (syscall 63 #f #f #f))
(define CL_LIBRARY
   (c-string
   (cond
      ((string-ci=? (ref uname 1) "Windows")  "opencl")
      ((string-ci=? (ref uname 1) "Linux")    "libOpenCL.so")
      ;"HP-UX"
      ;"SunOS"
      ;"Darwin"
      ;"FreeBSD"
      ;"CYGWIN_NT-5.2-WOW64"
      ;"MINGW32_NT-5.2"
      ;...
      (else
         (runtime-error "Unknown platform")))))

(define $ (dlopen CL_LIBRARY))
(if (not $)
   (runtime-error "Can't load CL library"))

(define char* type-string)
(define void* type-vector-raw)
(define size_t* type-vector-raw)
(define size_t type-int+)
(define CL_CALLBACK* type-port)

   (define clGetPlatformIDs     (dlsym $ cl_int "clGetPlatformIDs" cl_uint cl_platform_id* cl_uint*))
   (define clGetPlatformInfo    (dlsym $ cl_int "clGetPlatformInfo" cl_platform_id cl_platform_info type-int+ void* size_t*))
   (define clGetDeviceIDs       (dlsym $ cl_int "clGetDeviceIDs" cl_platform_id cl_device_type cl_uint cl_device_id* cl_uint*))
   (define clGetDeviceInfo      (dlsym $ cl_int "clGetDeviceInfo" cl_device_id cl_device_info size_t void* size_t*))

; Context APIs
   (define clCreateContext      (dlsym $ cl_context "clCreateContext" cl_context_properties* cl_uint cl_device_id* CL_CALLBACK* void* cl_int*))
;extern CL_API_ENTRY cl_context CL_API_CALL
;clCreateContext(const cl_context_properties * /* properties */,
;                cl_uint                       /* num_devices */,
;                const cl_device_id *          /* devices */,
;                void (CL_CALLBACK * /* pfn_notify */)(const char *, const void *, size_t, void *),
;                void *                        /* user_data */,
;                cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;
;
;extern CL_API_ENTRY cl_context CL_API_CALL
;clCreateContextFromType(const cl_context_properties * /* properties */,
;                        cl_device_type                /* device_type */,
;                        void (CL_CALLBACK *     /* pfn_notify*/ )(const char *, const void *, size_t, void *),
;                        void *                        /* user_data */,
;                        cl_int *                      /* errcode_ret */) CL_API_SUFFIX__VERSION_1_0;
;
;extern CL_API_ENTRY cl_int CL_API_CALL
;clRetainContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;
;
;extern CL_API_ENTRY cl_int CL_API_CALL
;clReleaseContext(cl_context /* context */) CL_API_SUFFIX__VERSION_1_0;
;
;extern CL_API_ENTRY cl_int CL_API_CALL
;clGetContextInfo(cl_context         /* context */,
;                 cl_context_info    /* param_name */,
;                 size_t             /* param_value_size */,
;                 void *             /* param_value */,
;                 size_t *           /* param_value_size_ret */) CL_API_SUFFIX__VERSION_1_0;

; Command Queue APIs
; ...

; Memory Object APIs
; ...

; Sampler APIs
; ...

; Program Object APIs
      (define char** (vm:or type-string #x40)) ; todo: change size_t* to array of integers
   (define clCreateProgramWithSource (dlsym $ cl_program "clCreateProgramWithSource" cl_context cl_uint char** size_t* cl_int*))

   (define clBuildProgram (dlsym $ cl_int "clBuildProgram" cl_program cl_uint cl_device_id* char* CL_CALLBACK* void*))

   (define clGetProgramBuildInfo (dlsym $ cl_int "clGetProgramBuildInfo" cl_program cl_device_id cl_program_build_info size_t void* size_t*))

))