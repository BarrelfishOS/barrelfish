/**
 * \file psci.c
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

#include <kernel.h>
#include <stddef.h>
#include <errno.h>

#include <arch/arm/smc_hvc.h>

#include <psci.h>


static uint64_t psci_use_hvc = 0;

static inline void psci_invoke(psci_fn_t fn, uintptr_t arg0, uintptr_t arg1,
                               uintptr_t arg2, struct arm_smc_hvc_retval *ret_val)
{
    /*
     * TODO: we need to distinguish between SMC and HVC instructions to
     *       call the PSCI function
     */
    if (psci_use_hvc)
        invoke_arm_hvc(fn, arg0, arg1, arg2, 0, 0, 0, 0, ret_val);
    else
        invoke_arm_smc(fn, arg0, arg1, arg2, 0, 0, 0, 0, ret_val);
}


static errval_t psci_error_to_barrelfish(struct arm_smc_hvc_retval retval)
{
    int32_t err = (int32_t)(retval.a0 & 0xffffffff);

    if (err >= 0) {
        return SYS_ERR_OK;
    }

    if (err == PSCI_ERRNO_NOT_SUPPORTED) {
        return PSCI_ERR_NOT_SUPPORTED;
    } else if (err == PSCI_ERRNO_INVALID_PARAMETER) {
        return PSCI_ERR_INVALID_PARAMETER;
    } else if (err == PSCI_ERRNO_DENIED) {
        return PSCI_ERR_DENIED;
    } else if (err == PSCI_ERRNO_ALREADY_ON) {
        return PSCI_ERR_ALREADY_ON;
    } else if (err == PSCI_ERRNO_ON_PENDING) {
        return PSCI_ERR_ON_PENDING;
    } else if (err == PSCI_ERRNO_INTERNAL_FAILURE) {
        return PSCI_ERR_INTERNAL_FAILURE;
    } else if (err == PSCI_ERRNO_NOT_PRESENT) {
        return PSCI_ERR_NOT_PRESENT;
    } else if (err == PSCI_ERRNO_DISABLED) {
        return PSCI_ERR_DISABLED;
    } else if (err == PSCI_ERRNO_INVALID_ADDRESS) {
        return PSCI_ERR_INVALID_ADDRESS;
    }

    return PSCI_ERR_UNKNOWN_ERROR;
}

/**
 * @brief Return the version of PSCI implemented.
 *
 * @param major returns the major version
 * @param minor returns the minor version
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_* on failure
 */
errval_t psci_version(uint16_t *major, uint16_t *minor)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_VERSION, 0, 0, 0, &retval);

    uint32_t version = (uint32_t)retval.a0;
    if (major) {
        *major = (version  >> 16) & 0xffff;
    }
    if (minor) {
        *minor = version & 0xffff;
    }

    return SYS_ERR_OK;
}

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
                          uintptr_t context_id)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_CPU_SUSPEND, power_state, entry_point, context_id, &retval);

    return psci_error_to_barrelfish(retval);
}

/**
 * @brief Power down the calling core.
 *
 * @return PSCI_ERR_DENIED on failure
 *         The call does not return on success
 *
 * This call is intended for use in hotplug. A core that is powered down by
 * CPU_OFF can only be powered up again in response to a CPU_ON.
 */
errval_t psci_cpu_off(void)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_CPU_OFF, 0, 0, 0, &retval);

    return psci_error_to_barrelfish(retval);
}

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
                     uintptr_t context_id)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_CPU_ON, target_cpu, entry_point, context_id, &retval);

    return psci_error_to_barrelfish(retval);
}

/**
 * @brief Places the core into an IMPLEMENTATION DEFINED low-power state
 *
 * @return on success, does not return
 *         PSCI_ERR_NOT_SUPPORTED
 *         PSCI_ERR_DENIED
 */
errval_t psci_cpu_freeze(void)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_CPU_FREEZE, 0, 0, 0, &retval);

    return psci_error_to_barrelfish(retval);
}

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
                                  uintptr_t context_id)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_CPU_DEFAULT_SUSPEND64, entry_point_address, context_id,
                0, &retval);

    return psci_error_to_barrelfish(retval);
}


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
                            psci_affinity_t *ret_info)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_AFFINITY_INFO64, target_affinity, lowest_affinity_level,
                0, &retval);

    errval_t err =  psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_info) {
        *ret_info = (psci_affinity_t)retval.a0;
    }

    return err;
}

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
errval_t psci_migrate(uintptr_t target_cpu)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_MIGRATE64, target_cpu, 0, 0, &retval);

    return psci_error_to_barrelfish(retval);
}

/**
 * @brief Optional. This function allows a caller to identify the level of
 *        multicore support present in the Trusted OS
 *
 * @param ret_migrate_type  Migration type capabilities
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_NOT_SUPPORTED if the operation is not supported
 */
errval_t psci_migrate_info_type(psci_migrate_t *ret_migrate_type)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_MIGRATE_INFO_TYPE, 0, 0, 0, &retval);

    errval_t err = psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_migrate_type) {
        *ret_migrate_type = (psci_migrate_t)(retval.a0 & 0xffffffff);
    }

    return err;
}


/**
 * @brief Optional. For a uniprocessor Trusted OS, this function returns the
 *        current resident core
 *
 * @param ret_mpdir     MPIDR based value of core where the Trusted OS is resident
 *
 * @return  SYS_ERR_OK on success
 *          PSCI_ERR_NOT_SUPPORTED if not supported
 */
errval_t psci_migrate_info_up_cpu(uintptr_t *ret_mpdir)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_MIGRATE_INFO_UP_CPU64, 0, 0, 0, &retval);

    errval_t err = psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_mpdir) {
        *ret_mpdir = retval.a0;
    }

    return err;
}

/**
 * @brief Shutdown the system.
 */
void psci_system_off(void)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_SYSTEM_OFF, 0, 0, 0, &retval);
    panic("SHOULD NOT BE REACHED!\n");
}

/**
 * @brief Reset the system.
 */
void psci_system_reset(void)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_SYSTEM_REST, 0, 0, 0, &retval);
    panic("SHOULD NOT BE REACHED!\n");
}

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
errval_t psci_system_suspend(lpaddr_t entry_point_address, uintptr_t context_id)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_SYSTEM_SUPSEND64, entry_point_address, context_id, 0,
                &retval);

    return psci_error_to_barrelfish(retval);
}

/**
 * @brief Query API to discover whether a specific PSCI function is implemented
 *
 * @param psci_fn_id            Function ID for a PSCI Function
 * @param ret_feature_flags     Returns a set of feature flags of the function
 *
 * @return SYS_ERR_OK on success
 *         PSCI_ERR_NOT_SUPPORTED if the function is not supported or invalid
 */
errval_t psci_features(uint32_t psci_fn_id, uint32_t *ret_feature_flags)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_PSCI_FEATURES, psci_fn_id, 0, 0, &retval);

    errval_t err = psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_feature_flags) {
        *ret_feature_flags = (uint32_t)(retval.a0 & 0x7ffffff);
    }

    return err;
}


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
                            psci_node_hw_state_t *ret_node_hw)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_NODE_HW_STATE64, target_cpu, power_level,
                0, &retval);

    errval_t err =  psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_node_hw) {
        *ret_node_hw = (psci_node_hw_state_t)retval.a0;
    }

    return err;
}


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
errval_t psci_set_suspend_mode(psci_suspend_mode_t mode)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_PSCI_SET_SUSPEND_MODE, mode, 0, 0, &retval);

    return psci_error_to_barrelfish(retval);
}

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
                             uintptr_t *ret_residency)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_PSCI_STAT_RESIDENCY64, target_cpu, power_state,
            0, &retval);

    errval_t err =  psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && ret_residency) {
        *ret_residency = retval.a0;
    }

    return err;
}

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
                         uintptr_t *count)
{
    struct arm_smc_hvc_retval retval;
    psci_invoke(PSCI_FN_PSCI_STAT_COUNT64, target_cpu, power_state,
            0, &retval);

    errval_t err =  psci_error_to_barrelfish(retval);
    if (err_is_ok(err) && count) {
        *count = retval.a0;
    }

    return err;
}

/**
 * Change the PSCI's conduit to use hvc instead of smc
 * @param use_hvc True, if hvc should be used
 */
void psci_set_use_hvc(uint64_t use_hvc)
{
    psci_use_hvc = use_hvc;
}
