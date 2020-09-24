package com.g2forge.reassert.contract.algorithm.worklicense.model.name;

import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Builder;
import lombok.Data;

@Data
@Builder(toBuilder = true)
public class LicenseWorkLicenseName implements IWorkLicenseName {
	protected final ILicenseTerm term;

	protected final ILicense license;
}
