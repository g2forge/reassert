package com.g2forge.reassert.contract;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.license.StandardLicenseDescriber;
import com.g2forge.reassert.contract.license.StandardLicenseParser;
import com.g2forge.reassert.contract.usage.StandardUsageDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;

@Service(IModule.class)
public class StandardModule implements IModule, ISingleton {
	protected static final StandardModule INSTANCE = new StandardModule();

	public static StandardModule create() {
		return INSTANCE;
	}

	private StandardModule() {}

	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.licenseParser(StandardLicenseParser.create());
		builder.describer(StandardLicenseDescriber.create());
		builder.describer(StandardUsageDescriber.create());
		return builder.build();
	}
}
