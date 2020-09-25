package com.g2forge.reassert.contract.algorithm.worklicense.model.rule;

import java.util.Collection;

import com.g2forge.reassert.contract.model.rule.IContractComparisonRules;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

public interface IWorkLicenseRules extends IContractComparisonRules {
	public Collection<ILicenseTerm> getUnknown();
}
