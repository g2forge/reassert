package com.g2forge.reassert.contract.model.licenseusage;

import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class CTNameUsageTerm implements ICTName {
	protected final IUsageTerm term;
}
