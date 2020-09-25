package com.g2forge.reassert.contract.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.api.module.IContext;

public interface IContractComparisonNameRenderContext extends ITextualRenderContext<IContractComparisonName, IContractComparisonNameRenderContext> {
	public IContext getContext();
}
