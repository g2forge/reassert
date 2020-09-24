package com.g2forge.reassert.contract.algorithm.worklicense.model.name;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class BaseWorkLicenseName implements IWorkLicenseName {
	protected final ILicenseTerm term;
}
