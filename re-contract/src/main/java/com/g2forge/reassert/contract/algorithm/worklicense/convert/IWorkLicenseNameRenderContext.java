package com.g2forge.reassert.contract.algorithm.worklicense.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.algorithm.worklicense.model.name.IWorkLicenseName;
import com.g2forge.reassert.core.api.module.IContext;

public interface IWorkLicenseNameRenderContext extends ITextualRenderContext<IWorkLicenseName, IWorkLicenseNameRenderContext> {
	public IContext getContext();
}
