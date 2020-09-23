package com.g2forge.reassert.contract.algorithm.licenseusage.convert;

import com.g2forge.enigma.backend.convert.textual.ITextualRenderContext;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.core.api.module.IContext;

public interface ILicenseUsageNameRenderContext extends ITextualRenderContext<ILicenseUsageName, ILicenseUsageNameRenderContext> {
	public IContext getContext();
}
