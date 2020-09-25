package com.g2forge.reassert.contract.model.name;

import com.g2forge.reassert.core.model.contract.terms.ITerm;

public interface IContractComparisonName {
	public IContractComparisonNameScheme getScheme();
	
	public ITerm getTerm();
}
