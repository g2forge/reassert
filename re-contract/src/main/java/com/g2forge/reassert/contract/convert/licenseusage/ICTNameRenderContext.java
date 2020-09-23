package com.g2forge.reassert.contract.convert.licenseusage;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.model.licenseusage.ICTName;
import com.g2forge.reassert.core.api.module.IContext;

public interface ICTNameRenderContext extends ITextualRenderContext<ICTName, ICTNameRenderContext> {
	public IContext getContext();
}
