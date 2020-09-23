package com.g2forge.reassert.contract.model.licenseusage;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class CTNameLicenseTerm implements ICTName {
	protected final ILicenseTerm term;
}
