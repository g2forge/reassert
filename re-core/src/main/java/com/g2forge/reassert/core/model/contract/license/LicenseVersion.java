package com.g2forge.reassert.core.model.contract.license;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class LicenseVersion {
	protected final int major;

	protected final int minor;
	
	protected final int patch;
}
