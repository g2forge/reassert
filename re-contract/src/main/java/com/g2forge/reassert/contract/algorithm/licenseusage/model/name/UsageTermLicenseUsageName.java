package com.g2forge.reassert.contract.algorithm.licenseusage.model.name;

import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class UsageTermLicenseUsageName implements ILicenseUsageName {
	protected final IUsageTerm term;
}
