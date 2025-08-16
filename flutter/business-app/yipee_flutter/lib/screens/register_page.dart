// ----- simulated register code -----

import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import '../services/auth_service.dart';
import '../widgets/legal_disclaimer.dart';
import 'home_page.dart';

class RegisterPage extends StatelessWidget {
  final _formKey = GlobalKey<FormBuilderState>();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Register')),
      body: SingleChildScrollView(
        padding: EdgeInsets.all(16.0),
        child: FormBuilder(
          key: _formKey,
          child: Column(
            children: [
              FormBuilderTextField(
                name: 'businessName',
                decoration: InputDecoration(labelText: 'Business Name'),
                validator: FormBuilderValidators.required(),
              ),
              FormBuilderTextField(
                name: 'industry',
                decoration: InputDecoration(labelText: 'Industry'),
                validator: FormBuilderValidators.required(),
              ),
              FormBuilderTextField(
                name: 'ownerNames',
                decoration: InputDecoration(labelText: 'Owner Names'),
                validator: FormBuilderValidators.required(),
              ),
              FormBuilderTextField(
                name: 'annualIncome',
                decoration: InputDecoration(labelText: 'Annual Income'),
                validator: FormBuilderValidators.required(),
                keyboardType: TextInputType.number,
              ),
              FormBuilderTextField(
                name: 'businessLocation',
                decoration: InputDecoration(labelText: 'Business Location'),
                validator: FormBuilderValidators.required(),
              ),
              FormBuilderSwitch(
                name: 'hasPhysicalLocation',
                title: Text('Has Physical Location'),
              ),
              FormBuilderTextField(
                name: 'email',
                decoration: InputDecoration(labelText: 'Email'),
                validator: FormBuilderValidators.compose([
                  FormBuilderValidators.required(),
                  FormBuilderValidators.email(),
                ]),
              ),
              FormBuilderTextField(
                name: 'password',
                decoration: InputDecoration(labelText: 'Password'),
                obscureText: true,
                validator: FormBuilderValidators.required(),
              ),
              FormBuilderTextField(
                name: 'recoveryEmail',
                decoration: InputDecoration(labelText: 'Recovery Email'),
                validator: FormBuilderValidators.email(),
              ),
              FormBuilderTextField(
                name: 'recoveryPhone',
                decoration: InputDecoration(labelText: 'Recovery Phone'),
                keyboardType: TextInputType.phone,
              ),
              SizedBox(height: 20),
              LegalDisclaimer(),
              SizedBox(height: 20),
              ElevatedButton(
                child: Text('Register'),
                onPressed: () async {
                  if (_formKey.currentState!.saveAndValidate()) {
                    final formData = _formKey.currentState!.value;
                    final authService = Provider.of<AuthService>(context, listen: false);
                    final success = await authService.register(
                      formData['email'],
                      formData['password'],
                    );
                    if (success) {
                      Navigator.pushReplacement(
                        context,
                        MaterialPageRoute(builder: (context) => HomePage()),
                      );
                    } else {
                      ScaffoldMessenger.of(context).showSnackBar(
                        SnackBar(content: Text('Registration failed')),
                      );
                    }
                  }
                },
              ),
            ],
          ),
        ),
      ),
    );
  }
}

// ----- actual firebase linking code -----

// import 'package:flutter/material.dart';
// import 'package:provider/provider.dart';
// import 'package:flutter_form_builder/flutter_form_builder.dart';
// import '../services/auth_service.dart';
// import '../widgets/legal_disclaimer.dart';
// import 'home_page.dart';

// class RegisterPage extends StatelessWidget {
//   final _formKey = GlobalKey<FormBuilderState>();

//   @override
//   Widget build(BuildContext context) {
//     return Scaffold(
//       appBar: AppBar(title: Text('Register')),
//       body: SingleChildScrollView(
//         padding: EdgeInsets.all(16.0),
//         child: FormBuilder(
//           key: _formKey,
//           child: Column(
//             children: [
//               FormBuilderTextField(
//                 name: 'businessName',
//                 decoration: InputDecoration(labelText: 'Business Name'),
//                 validator: FormBuilderValidators.required(),
//               ),
//               FormBuilderTextField(
//                 name: 'industry',
//                 decoration: InputDecoration(labelText: 'Industry'),
//                 validator: FormBuilderValidators.required(),
//               ),
//               FormBuilderTextField(
//                 name: 'ownerNames',
//                 decoration: InputDecoration(labelText: 'Owner Names'),
//                 validator: FormBuilderValidators.required(),
//               ),
//               FormBuilderTextField(
//                 name: 'annualIncome',
//                 decoration: InputDecoration(labelText: 'Annual Income'),
//                 validator: FormBuilderValidators.required(),
//                 keyboardType: TextInputType.number,
//               ),
//               FormBuilderTextField(
//                 name: 'businessLocation',
//                 decoration: InputDecoration(labelText: 'Business Location'),
//                 validator: FormBuilderValidators.required(),
//               ),
//               FormBuilderSwitch(
//                 name: 'hasPhysicalLocation',
//                 title: Text('Has Physical Location'),
//               ),
//               FormBuilderTextField(
//                 name: 'email',
//                 decoration: InputDecoration(labelText: 'Email'),
//                 validator: FormBuilderValidators.compose([
//                   FormBuilderValidators.required(),
//                   FormBuilderValidators.email(),
//                 ]),
//               ),
//               FormBuilderTextField(
//                 name: 'password',
//                 decoration: InputDecoration(labelText: 'Password'),
//                 obscureText: true,
//                 validator: FormBuilderValidators.required(),
//               ),
//               FormBuilderTextField(
//                 name: 'recoveryEmail',
//                 decoration: InputDecoration(labelText: 'Recovery Email'),
//                 validator: FormBuilderValidators.email(),
//               ),
//               FormBuilderTextField(
//                 name: 'recoveryPhone',
//                 decoration: InputDecoration(labelText: 'Recovery Phone'),
//                 keyboardType: TextInputType.phone,
//               ),
//               SizedBox(height: 20),
//               LegalDisclaimer(),
//               SizedBox(height: 20),
//               ElevatedButton(
//                 child: Text('Register'),
//                 onPressed: () async {
//                   if (_formKey.currentState!.saveAndValidate()) {
//                     final formData = _formKey.currentState!.value;
//                     final authService = Provider.of<AuthService>(context, listen: false);
//                     final result = await authService.register(
//                       formData['email'],
//                       formData['password'],
//                     );
//                     if (result != null) {
//                       // Here you would typically save the additional business information
//                       // to a database or Firestore collection
//                       Navigator.pushReplacement(
//                         context,
//                         MaterialPageRoute(builder: (context) => HomePage()),
//                       );
//                     } else {
//                       ScaffoldMessenger.of(context).showSnackBar(
//                         SnackBar(content: Text('Registration failed')),
//                       );
//                     }
//                   }
//                 },
//               ),
//             ],
//           ),
//         ),
//       ),
//     );
//   }
// }