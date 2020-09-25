package com.g2forge.reassert.standard;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTermsLoader;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTermsLoader;

@Service(IModule.class)
public class StandardTestModule implements IModule, ISingleton {
	protected static final StandardTestModule INSTANCE = new StandardTestModule();

	public static StandardTestModule create() {
		return INSTANCE;
	}

	private StandardTestModule() {}

	@ReassertLegalOpinion
	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.termsLoader(StandardLicenseTermsLoader.create());
		builder.termsLoader(StandardUsageTermsLoader.create());
		return builder.build();
	}
}
