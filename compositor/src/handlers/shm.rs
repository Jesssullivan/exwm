//! wl_shm (shared memory) handler.

use crate::state::EwwmState;
use smithay::{delegate_shm, wayland::shm::ShmHandler};

impl ShmHandler for EwwmState {
    fn shm_state(&self) -> &smithay::wayland::shm::ShmState {
        &self.shm_state
    }
}

delegate_shm!(EwwmState);
