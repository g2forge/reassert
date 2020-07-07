package com.g2forge.reassert.license;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;

@Service(IModule.class)
public class LicenseModule implements IModule, ISingleton {
	protected static final LicenseModule INSTANCE = new LicenseModule();

	public static LicenseModule create() {
		return INSTANCE;
	}

	private LicenseModule() {}

	@Override
	public Loaded load(IContext context) {
		final LoadedBuilder builder = IModule.Loaded.builder();
		builder.licenseParser(StandardLicenseParser.create());
		builder.describer(StandardLicenseDescriber.create());
		return builder.build();
	}
}
