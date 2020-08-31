package com.g2forge.reassert.standard;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.module.IModule;
import com.g2forge.reassert.core.api.module.IModule.Loaded.LoadedBuilder;
import com.g2forge.reassert.standard.api.scanner.LicenseFileScanner;
import com.g2forge.reassert.standard.model.contract.license.parser.StandardLicenseParser;

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
		builder.scanner(new LicenseFileScanner(context));
		builder.licenseParser(StandardLicenseParser.create());
		return builder.build();
	}
}
