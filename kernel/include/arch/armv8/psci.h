/**
 * \file
 * \brief
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARM_PSCI_H_
#define ARM_PSCI_H_


/**
 * Function Prototypes for the PSCI interface
 */
typedef enum {
    PSCI_FN_VERSION               = 0x84000000,
    PSCI_FN_CPU_SUSPEND           = 0x84000001,          //!< PSCI_FN_CPU_SUSPEND
    PSCI_FN_CPU_SUSPEND64         = 0xC4000001,          //!< PSCI_FN_CPU_SUSPEND
    PSCI_FN_CPU_OFF               = 0x84000002,              //!< PSCI_FN_CPU_OFF
    PSCI_FN_CPU_ON                = 0x84000003,               //!< PSCI_FN_CPU_ON
    PSCI_FN_CPU_ON64              = 0xC4000003,               //!< PSCI_FN_CPU_ON
    PSCI_FN_AFFINITY_INFO         = 0x84000004,        //!< PSCI_FN_AFFINITY_INFO
    PSCI_FN_AFFINITY_INFO64       = 0xC4000004,        //!< PSCI_FN_AFFINITY_INFO
    PSCI_FN_MIGRATE               = 0x84000005,              //!< PSCI_FN_MIGRATE
    PSCI_FN_MIGRATE64             = 0xC4000005,              //!< PSCI_FN_MIGRATE
    PSCI_FN_MIGRATE_INFO_TYPE     = 0x84000006,    //!< PSCI_FN_MIGRATE_INFO_TYPE
    PSCI_FN_MIGRATE_INFO_UP_CPU   = 0x84000007,  //!< PSCI_FN_MIGRATE_INFO_UP_CPU
    PSCI_FN_MIGRATE_INFO_UP_CPU64 = 0xC4000007,  //!< PSCI_FN_MIGRATE_INFO_UP_CPU
    PSCI_FN_SYSTEM_OFF            = 0x84000008,           //!< PSCI_FN_SYSTEM_OFF
    PSCI_FN_SYSTEM_REST           = 0x84000009,          //!< PSCI_FN_SYSTEM_REST
    PSCI_FN_PSCI_FEATURES         = 0x8400000A,//!< PSCI_FN_PSCI_FEATURES
    PSCI_FN_CPU_FREEZE            = 0x8400000B,           //!< PSCI_FN_CPU_FREEZE
    PSCI_FN_CPU_DEFAULT_SUSPEND   = 0x8400000C,  //!< PSCI_FN_CPU_DEFAULT_SUSPEND
    PSCI_FN_CPU_DEFAULT_SUSPEND64 = 0xC400000C,  //!< PSCI_FN_CPU_DEFAULT_SUSPEND
    PSCI_FN_NODE_HW_STATE         = 0x8400000D,        //!< PSCI_FN_NODE_HW_STATE
    PSCI_FN_NODE_HW_STATE64       = 0xC400000D,        //!< PSCI_FN_NODE_HW_STATE
    PSCI_FN_SYSTEM_SUPSEND        = 0x8400000E,       //!< PSCI_FN_SYSTEM_SUPSEND
    PSCI_FN_SYSTEM_SUPSEND64      = 0xC400000E,       //!< PSCI_FN_SYSTEM_SUPSEND
    PSCI_FN_PSCI_SET_SUSPEND_MODE = 0x8400000F,//!< PSCI_FN_PSCI_SET_SUSPEND_MODE
    PSCI_FN_PSCI_STAT_RESIDENCY   = 0x84000010,  //!< PSCI_FN_PSCI_STAT_RESIDENCY
    PSCI_FN_PSCI_STAT_RESIDENCY64 = 0xC4000010,  //!< PSCI_FN_PSCI_STAT_RESIDENCY
    PSCI_FN_PSCI_STAT_COUNT       = 0x84000011,      //!< PSCI_FN_PSCI_STAT_COUNT
    PSCI_FN_PSCI_STAT_COUNT64     = 0x84000012,      //!< PSCI_FN_PSCI_STAT_COUNT
} psci_fn_t;


/**
 *
 * Table 7 defines the values for error codes used with PSCI functions. All
 * errors are considered to be 32-bit signed integers. Therefore, when using the
 * SMC64 calling convention, the upper word will be zero.
 */
typedef enum {
    PSCI_ERRNO_SUCCESS = 0,          //!< PSCI_ERRNO_SUCCESS
    PSCI_ERRNO_NOT_SUPPORTED = -1,   //!< PSCI_ERRNO_NOT_SUPPORTED
    PSCI_ERRNO_INVALID_PARAMETER = -2,//!< PSCI_ERRNO_INVALID_PARAMTER
    PSCI_ERRNO_DENIED = -3,          //!< PSCI_ERRNO_DENIED
    PSCI_ERRNO_ALREADY_ON  =-4,      //!< PSCI_ERRNO_ALREADY_ON
    PSCI_ERRNO_ON_PENDING = -5,      //!< PSCI_ERRNO_ON_PENDING
    PSCI_ERRNO_INTERNAL_FAILURE = -6,//!< PSCI_ERRNO_INTERNAL_FAILURE
    PSCI_ERRNO_NOT_PRESENT = -7,     //!< PSCI_ERRNO_NOT_PRESENT
    PSCI_ERRNO_DISABLED = -8,        //!< PSCI_ERRNO_DISABLED
    PSCI_ERRNO_INVALID_ADDRESS = -9, //!< PSCI_ERRNO_INVALID_ADDRESS
    PSCI_ERRNO_LAST = -10
} psci_errno_t;

/**
 *
 */
typedef enum {
    PSCI_AFFINITY_ON = 0,        //!< At least one core in the instance is ON
    PSCI_AFFINITY_OFF = 1,       //!< All cores in the affinity instance are OFF
    PSCI_AFFINITY_ON_PENDING = 2,//!< instance is transitioning to an ON state
    PSCI_AFFINITY_INVALID = 3,   //!< Invalid affinity state
} psci_affinity_t;

/**
 *
 */
typedef enum {
    PSCI_MIGRATE_CAPABLE = 0,    //!< Uniprocessor migrate capable Trusted OS
    PSCI_MIGRATE_NOT_CAPABLE = 1,//!< Uniprocessor not migrate capable Trusted OS
    PSCI_MIGRATE_NOT_PRESENT = 2,//!< Trusted OS is either not present
    PSCI_MIGRATE_INVALID = 2,    //!< Invalid migration type
} psci_migrate_t;

typedef enum {
    PSCI_NODE_HW_ON = 0,
    PSCI_NODE_HW_OFF = 1,
    PSCI_NODE_HW_STANDBY = 2,
    PSCI_NODE_HW_INVALID = 3,
} psci_node_hw_state_t;

typedef enum {
    PSCI_SUSPEND_MODE_PLATFORM = 0,
    PSCI_SUSPEND_MODE_OS       = 1,
} psci_suspend_mode_t;


/**
 * @brief Return the version of PSCI implemented.
 *
 * @param major returns the major version
 * @param minor returns the minor version
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_* on failure
 */
errval_t psci_version(uint16_t *major, uint16_t *minor);

/**
 * @brief Suspend execution on a core or higher level topology node.
 *
 * @param power_state   the power state to
 * @param entry_point   Physical address of the location to resume execution
 * @param context_id    Value left in x0/r0 when resuming execution
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_INVALID_PARAMTER
 *         PSCI_ERR_INVALID_ADDRESS
 *         PSCI_ERR_DENIED
 *
 * This call is intended for use in idle subsystems where the core is expected
 * to return to execution through a wakeup event. See section 5.4.
 */
errval_t psci_cpu_suspend(uint32_t power_state, lpaddr_t entry_point,
                          uintptr_t context_id);

/**
 * @brief Power down the calling core.
 *
 * @return PSCI_ERR_DENIED on failure
 *         The call does not return on success
 *
 * This call is intended for use in hotplug. A core that is powered down by
 * CPU_OFF can only be powered up again in response to a CPU_ON.
 */
errval_t psci_cpu_off(void);

/**
 * @brief Power up a core.
 *
 * @param target_cpu    MPIDR of the target processors
 * @param entry_point   Address at which the core must commence execution
 * @param context_id    Value left in x0/r0 when resuming execution
 *
 * @return  SYS_ERR_OK on success
 *          PSCI_ERR_INVALID_PARAMETERS
 *          PSCI_ERR_INVALID_ADDRESS
 *          PSCI_ERR_ALREADY_ON
 *          PSCI_ERR_ON_PENDING
 *          PSCI_ERR_INTERNAL_FAILURE
 *
 * This call is used to power up cores that either:
 * - Have not yet been booted into the calling supervisory software.
 * - Have been previously powered down with a CPU_OFF call.
 */
errval_t psci_cpu_on(uintptr_t target_cpu, lpaddr_t entry_point,
                     uintptr_t context_id);

/**
 * @brief Places the core into an IMPLEMENTATION DEFINED low-power state
 *
 * @return on success, does not return
 *         PSCI_ERR_NOT_SUPPORTED
 *         PSCI_ERR_DENIED
 */
errval_t psci_cpu_freeze(void);

/**
 * @brief Will place a core into an IMPLEMENTATION DEFINED low-power state
 *
 * @param entry_point_address   Address to be executed when waking up
 * @param context_id            Context pointer to be left in x0/r0
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_INVALID_ADDRESS
 */
errval_t psci_cpu_default_suspend(lpaddr_t entry_point_address,
                                  uintptr_t context_id);


/**
 * @brief Enable the caller to request status of an affinity instance
 *
 * @param target_affinity       MPDIR of the target cpu
 * @param lowest_affinity_level Denotes the lowest valid affinity level field
 * @param ret_info
 *
 * @return
 */
errval_t psci_affinity_info(uintptr_t target_affinity,
                            uint32_t lowest_affinity_level,
                            psci_affinity_t *ret_info);

/**
 * @brief Optional. This is used to ask a uniprocessor Trusted OS to migrate
 *        its context to a specific core.
 *
 * @param target_cpu    MPDIR of the target core
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_NOT_SUPPORTED
 *         PSCI_ERR_INVALID_PARAMETERS
 *         PSCI_ERR_DENIED
 *         PSCI_ERR_INTERNAL_FAILURE
 *         PSCI_ERR_NOT_PRESENT
 */
errval_t psci_migrate(uintptr_t target_cpu);

/**
 * @brief Optional. This function allows a caller to identify the level of
 *        multicore support present in the Trusted OS
 *
 * @param ret_migrate_type  Migration type capabilities
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_NOT_SUPPORTED if the operation is not supported
 */
errval_t psci_migrate_info_type(psci_migrate_t *ret_migrate_type);


/**
 * @brief Optional. For a uniprocessor Trusted OS, this function returns the
 *        current resident core
 *
 * @param ret_mpdir     MPIDR based value of core where the Trusted OS is resident
 *
 * @return  SYS_ERR_OK on success
 *          PSCI_ERR_NOT_SUPPORTED if not supported
 */
errval_t psci_migrate_info_up_cpu(uintptr_t *ret_mpdir);

/**
 * @brief Shutdown the system.
 */
void psci_system_off(void)__attribute__((noreturn));

/**
 * @brief Reset the system.
 */
void psci_system_reset(void)__attribute__((noreturn));

/**
 * @brief Used to implement suspend to RAM. The semantics are equivalent to a
 *        CPU_SUSPEND to the deepest low-power state.
 *
 * @param entry_point_address   physical address to be executed when returning
 * @param context_id            context id to be stored in x0/r0
 *
 * @return does not retturn on success
 *         PSCI_ERR_NOT_SUPPORTED
 *         PSCI_ERR_INVALID_ADDRESS
 *         PSCI_ERR_ALREADY_ON
 */
errval_t psci_system_suspend(lpaddr_t entry_point_address, uintptr_t context_id);

/**
 * @brief Query API to discover whether a specific PSCI function is implemented
 *
 * @param psci_fn_id            Function ID for a PSCI Function
 * @param ret_feature_flags     Returns a set of feature flags of the function
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_NOT_SUPPORTED if the function is not supported or invalid
 */
errval_t psci_features(uint32_t psci_fn_id, uint32_t *ret_feature_flags);


/**
 * @brief This API is intended to return the true HW state of a node in the
 *        power domain topology of the system
 *
 * @param target_cpu    Target CPU MPDIR
 * @param power_level   Power domain level for the node
 * @param ret_node_hw   return the power node hw state
 *
 * @return  SYS_ERR_OK on success
 *          PSCI_ERR_NOT_SUPPORTED
 *          PSCI_ERR_INVALID_PARAMETERS
 */
errval_t psci_node_hw_state(uintptr_t target_cpu, uint32_t power_level,
                            psci_node_hw_state_t *ret_node_hw);


/**
 * @brief This API allows setting the mode used by CPU_SUSPEND to coordinate
 *        power states.
 *
 * @param mode  Mode to put the system in
 *
 * @return SYS_ERR_OK on success,
 *         PSCI_ERR_NOT_SUPPORTED
 *         PSCI_ERR_INVALID_PARAMETERS
 *         PSCI_ERR_DENIED
 */
errval_t psci_set_suspend_mode(psci_suspend_mode_t mode);

/**
 * @brief Returns the amount of time the platform has spent in the given power
 *        state since cold boot.
 *
 * @param target_cpu    target CPU MPDIR
 * @param power_state   power state to query
 * @param ret_residency Returns the amount of time, in microseconds, spent in state
 *
 * @return  SYS_ERR_OK
 */
errval_t psci_stat_residency(uintptr_t target_cpu, uint32_t power_state,
                             uintptr_t *ret_residency);

/**
 * @brief Return the number of times the platform has used the given power state
 *        since cold boot.
 *
 * @param target_cpu    target CPU MPDIR
 * @param power_state   power state to query
 * @param count         returns the count
 *
 * @return SYS_ERR_OK
 */
errval_t psci_stat_count(uintptr_t target_cpu, uint32_t power_state,
                         uintptr_t *count);

/**
 * Change the PSCI's conduit to use hvc instead of smc
 * @param use_hvc True, if hvc should be used
 */
void psci_set_use_hvc(uint64_t use_hvc);

#endif
