package com.g2forge.reassert.contract.algorithm.worklicense.model.rule;

import com.g2forge.reassert.contract.algorithm.worklicense.model.WorkLicenseNameScheme;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.contract.model.rule.AContractComparisonRules;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

public abstract class AWorkLicenseRules extends AContractComparisonRules<ILicenseTerm, ILicenseTerm> implements IWorkLicenseRules {
	@Override
	protected IContractComparisonScheme<ILicenseTerm, ILicenseTerm> getScheme() {
		return WorkLicenseNameScheme.create();
	}
}
